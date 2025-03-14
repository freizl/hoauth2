{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.TokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson (FromJSON)
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 (
  ClientAuthenticationMethod (..),
  OAuth2,
  PostBody,
  TokenResponse,
  uriToRequest,
 )
import Network.OAuth.OAuth2.TokenRequest (
  TokenResponseError,
  addBasicAuth,
  addDefaultRequestHeaders,
  handleOAuth2TokenResponse,
  parseResponseFlexible,
 )
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils
import URI.ByteString (URI)

class HasTokenRequestClientAuthenticationMethod a where
  getClientAuthenticationMethod :: a -> ClientAuthenticationMethod

-- | Only Authorization Code Grant involves a Exchange Token (Authorization Code).
-- ResourceOwnerPassword and Client Credentials make token request directly.
data NoNeedExchangeToken = NoNeedExchangeToken

class (HasOAuth2Key a, HasTokenRequestClientAuthenticationMethod a) => HasTokenRequest a where
  -- Each GrantTypeFlow has slightly different request parameter to /token endpoint.
  data TokenRequest a
  type ExchangeTokenInfo a

  -- | Only 'AuthorizationCode flow (but not resource owner password nor client credentials) will use 'ExchangeToken' in the token request
  -- create type family to be explicit on it.
  -- with 'type instance WithExchangeToken a b = b' implies no exchange token
  -- v.s. 'type instance WithExchangeToken a b = ExchangeToken -> b' implies needing an exchange token
  -- type WithExchangeToken a b
  mkTokenRequestParam :: a -> ExchangeTokenInfo a -> TokenRequest a

-------------------------------------------------------------------------------
--                               Token Request                               --
-------------------------------------------------------------------------------

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.3
conduitTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  ExchangeTokenInfo a ->
  ExceptT TokenResponseError m TokenResponse
conduitTokenRequest idpApp mgr exchangeToken = do
  conduitTokenRequestInternal idpApp mgr (exchangeToken, Nothing)

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
  conduitTokenRequestInternal idpApp mgr (exchangeToken, Just codeVerifier)

-------------------------------------------------------------------------------
--                              Internal helpers                             --
-------------------------------------------------------------------------------

conduitTokenRequestInternal ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  (ExchangeTokenInfo a, Maybe CodeVerifier) ->
  ExceptT TokenResponseError m TokenResponse
conduitTokenRequestInternal IdpApplication {..} mgr (exchangeToken, codeVerifier) =
  let req = mkTokenRequestParam application exchangeToken
      key = mkOAuth2Key application
      body =
        unionMapsToQueryParams
          [ toQueryParam req
          , toQueryParam codeVerifier
          ]
   in doTokenRequestInternal
        (getClientAuthenticationMethod application)
        mgr
        key
        (idpTokenEndpoint idp)
        body

doTokenRequestInternal ::
  (MonadIO m, FromJSON a) =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | URL
  URI ->
  -- | Request body.
  PostBody ->
  -- | Response as ByteString
  ExceptT TokenResponseError m a
doTokenRequestInternal clientAuthMethod manager oa url body = do
  resp <- ExceptT . liftIO $ fmap handleOAuth2TokenResponse go
  case parseResponseFlexible resp of
    Right obj -> return obj
    Left e -> throwE e
  where
    updateAuthHeader =
      case clientAuthMethod of
        ClientSecretBasic -> addBasicAuth oa
        ClientSecretPost -> id
        ClientAssertionJwt -> id

    go = do
      req <- uriToRequest url
      let req' = (updateAuthHeader . addDefaultRequestHeaders) req
      httpLbs (urlEncodedBody body req') manager
