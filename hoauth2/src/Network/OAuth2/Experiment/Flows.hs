{-# LANGUAGE FlexibleContexts #-}

-- | Module implementing various OAuth2 flow types and their request/response handling.
-- Provides support for:
--
--   * Authorization Code Grant
--   * Device Authorization Grant
--   * PKCE Extension
--   * Token Refresh
--   * User Info Endpoints
module Network.OAuth2.Experiment.Flows where

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON)
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe
import Network.HTTP.Client.Contrib
import Network.HTTP.Conduit
import Network.OAuth2
import Network.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
import Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Grants.AuthorizationCode
import Network.OAuth2.Experiment.Grants.DeviceAuthorization
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils
import URI.ByteString hiding (UserInfo)

-------------------------------------------------------------------------------
--                           Authorization Requests                          --
-------------------------------------------------------------------------------

-- | Constructs an Authorization Code request URI according to
-- <https://www.rfc-editor.org/rfc/rfc6749#section-4.1.1 RFC 6749 Section 4.1.1>.
--
-- The generated URI includes:
--   * client_id
--   * response_type (always "code")
--   * redirect_uri
--   * state (if provided)
--   * scope (if provided)
mkAuthorizationRequest :: IdpApplication i AuthorizationCodeApplication -> URI
mkAuthorizationRequest idpApp =
  let req = mkAuthorizationRequestParam (application idpApp)
      allParams =
        map (bimap tlToBS tlToBS) $
          Map.toList $
            toQueryParam req
   in appendQueryParams allParams $
        idpAuthorizeEndpoint (idp idpApp)

-- | Constructs an Authorization Code request URI with PKCE support according to
-- <https://datatracker.ietf.org/doc/html/rfc7636 RFC 7636>.
--
-- Returns both the authorization URI and the generated code verifier.
-- The code verifier must be stored securely for later use in the token request.
mkPkceAuthorizeRequest ::
  MonadIO m =>
  IdpApplication i AuthorizationCodeApplication ->
  m (URI, CodeVerifier)
mkPkceAuthorizeRequest IdpApplication {..} = do
  (req, codeVerifier) <- mkPkceAuthorizeRequestParam application
  let allParams = map (bimap tlToBS tlToBS) $ Map.toList $ toQueryParam req
  let url =
        appendQueryParams allParams $
          idpAuthorizeEndpoint idp
  pure (url, codeVerifier)

-------------------------------------------------------------------------------
--                                Device Auth                                --
-------------------------------------------------------------------------------

-- | Makes Device Authorization Request
-- https://www.rfc-editor.org/rfc/rfc8628#section-3.1
conduitDeviceAuthorizationRequest ::
  MonadIO m =>
  IdpApplication i DeviceAuthorizationApplication ->
  Manager ->
  ExceptT BSL.ByteString m DeviceAuthorizationResponse
conduitDeviceAuthorizationRequest IdpApplication {..} mgr = do
  case idpDeviceAuthorizationEndpoint idp of
    Nothing -> throwE "[conduitDeviceAuthorizationRequest] Device Authorization Flow is not supported: missing device_authorization_endpoint."
    Just deviceAuthEndpoint -> do
      let deviceAuthReq = mkDeviceAuthorizationRequestParam application
          body = unionMapsToQueryParams [toQueryParam deviceAuthReq]
      ExceptT . liftIO $ do
        req <- addDefaultRequestHeaders <$> uriToRequest deviceAuthEndpoint
        -- Note: Missing clientId indicates ClientSecretBasic authentication method
        -- should be used. See Network.OAuth2.Experiment.Grants.DeviceAuthorization
        let req' = case darClientId deviceAuthReq of
              Nothing -> addClientAuthToHeader application req
              Just _ -> req
        resp <- httpLbs (urlEncodedBody body req') mgr
        pure $ first ("[conduitDeviceAuthorizationRequest] " <>) $ handleResponseJSON resp

-- | Polls for a token using the device authorization flow.
--
-- This implements the polling mechanism described in
-- <https://www.rfc-editor.org/rfc/rfc8628#section-3.5 RFC 8628 Section 3.5>.
-- Handles automatic retries and interval adjustments based on IdP responses.
pollDeviceTokenRequest ::
  MonadIO m =>
  IdpApplication i DeviceAuthorizationApplication ->
  Manager ->
  DeviceAuthorizationResponse ->
  ExceptT TokenResponseError m TokenResponse
pollDeviceTokenRequest idpApp mgr deviceAuthResp = do
  pollDeviceTokenRequestInternal
    idpApp
    mgr
    (deviceCode deviceAuthResp)
    (fromMaybe 5 $ interval deviceAuthResp)

pollDeviceTokenRequestInternal ::
  MonadIO m =>
  IdpApplication i DeviceAuthorizationApplication ->
  Manager ->
  DeviceCode ->
  Int ->
  -- | Polling Interval
  ExceptT TokenResponseError m TokenResponse
pollDeviceTokenRequestInternal idpApp mgr deviceCode intervalSeconds = do
  resp <- runExceptT (conduitTokenRequest idpApp mgr deviceCode)
  case resp of
    Left trRespError -> do
      case tokenResponseError trRespError of
        -- TODO: Didn't have a good idea to expand the error code
        -- specifically for device token request flow
        -- Device Token Response additional error code: https://www.rfc-editor.org/rfc/rfc8628#section-3.5
        UnknownErrorCode "authorization_pending" -> do
          liftIO $ threadDelay $ intervalSeconds * 1000000
          pollDeviceTokenRequestInternal idpApp mgr deviceCode intervalSeconds
        UnknownErrorCode "slow_down" -> do
          let newIntervalSeconds = intervalSeconds + 5
          liftIO $ threadDelay $ newIntervalSeconds * 1000000
          pollDeviceTokenRequestInternal idpApp mgr deviceCode newIntervalSeconds
        _ -> throwE trRespError
    Right v -> pure v

-------------------------------------------------------------------------------
--                               Token Request                               --
-------------------------------------------------------------------------------

-- | Sends a token request according to
-- <https://www.rfc-editor.org/rfc/rfc6749#section-4.1.3 RFC 6749 Section 4.1.3>.
--
-- This is used for exchanging authorization codes, device codes, or other
-- grant types for access tokens.
conduitTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  ExchangeTokenInfo a ->
  ExceptT TokenResponseError m TokenResponse
conduitTokenRequest idpApp mgr exchangeToken = do
  let req = mkTokenRequestParam (application idpApp) exchangeToken
      body =
        unionMapsToQueryParams
          [ toQueryParam req
          ]
   in conduitTokenRequestInternal idpApp mgr body

-------------------------------------------------------------------------------
--                             PKCE Token Request                            --
-------------------------------------------------------------------------------

-- | https://datatracker.ietf.org/doc/html/rfc7636#section-4.5
conduitPkceTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  (ExchangeTokenInfo a, CodeVerifier) ->
  ExceptT TokenResponseError m TokenResponse
conduitPkceTokenRequest idpApp mgr (exchangeToken, codeVerifier) =
  let req = mkTokenRequestParam (application idpApp) exchangeToken
      body =
        unionMapsToQueryParams
          [ toQueryParam req
          , toQueryParam codeVerifier
          ]
   in conduitTokenRequestInternal idpApp mgr body

-------------------------------------------------------------------------------
--                              Refresh Token                               --
-------------------------------------------------------------------------------

-- | Makes a Refresh Token Request according to
-- <https://www.rfc-editor.org/rfc/rfc6749#section-6 RFC 6749 Section 6>.
--
-- Used to obtain a new access token using a refresh token.
conduitRefreshTokenRequest ::
  (MonadIO m, HasRefreshTokenRequest a) =>
  IdpApplication i a ->
  Manager ->
  OAuth2.RefreshToken ->
  ExceptT TokenResponseError m TokenResponse
conduitRefreshTokenRequest ia mgr rt =
  let tokenReq = mkRefreshTokenRequestParam (application ia) rt
      body = unionMapsToQueryParams [toQueryParam tokenReq]
   in conduitTokenRequestInternal ia mgr body

-------------------------------------------------------------------------------
--                                 User Info                                 --
-------------------------------------------------------------------------------

-- | Makes a standard request to the userinfo endpoint using GET method.
--
-- This is commonly used with OpenID Connect providers to fetch
-- user profile information using an access token.
conduitUserInfoRequest ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
conduitUserInfoRequest = conduitUserInfoRequestWithCustomMethod authGetJSON

-- | Makes a request to the userinfo endpoint using a custom HTTP method.
--
-- Some IdPs may require different HTTP methods (instead of GET) or custom headers
-- for fetching user information. This function provides that flexibility.
conduitUserInfoRequestWithCustomMethod ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  ( Manager ->
    AccessToken ->
    URI ->
    ExceptT BSL.ByteString m b
  ) ->
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
conduitUserInfoRequestWithCustomMethod fetchMethod IdpApplication {..} mgr at =
  fetchMethod mgr at (idpUserInfoEndpoint idp)

-------------------------------------------------------------------------------
--                              Internal helpers                             --
-------------------------------------------------------------------------------

conduitTokenRequestInternal ::
  ( MonadIO m
  , HasClientAuthenticationMethod a
  , FromJSON b
  ) =>
  IdpApplication i a ->
  -- | HTTP connection manager.
  Manager ->
  -- | Request body.
  PostBody ->
  -- | Response as ByteString
  ExceptT TokenResponseError m b
conduitTokenRequestInternal IdpApplication {..} manager body = do
  let clientAuthMethod = getClientAuthenticationMethod application
      url = idpTokenEndpoint idp
      updateAuthHeader =
        case clientAuthMethod of
          ClientSecretBasic -> addClientAuthToHeader application
          ClientSecretPost -> id
          ClientAssertionJwt -> id
      go = do
        req <- uriToRequest url
        let req' = (updateAuthHeader . addDefaultRequestHeaders) req
        httpLbs (urlEncodedBody body req') manager
  resp <- ExceptT . liftIO $ fmap handleOAuth2TokenResponse go
  case parseResponseFlexible resp of
    Right obj -> return obj
    Left e -> throwE e
