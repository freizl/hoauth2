{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.TokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils

-------------------------------------------------------------------------------
--                               Token Request                               --
-------------------------------------------------------------------------------

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

-- | Make Token Request
-- https://www.rfc-editor.org/rfc/rfc6749#section-4.1.3
conduitTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  ExchangeTokenInfo a ->
  ExceptT TokenResponseError m OAuth2Token
conduitTokenRequest IdpApplication {..} mgr exchangeToken = do
  let tokenReq = mkTokenRequestParam application exchangeToken
      key = mkOAuth2Key application
      body = unionMapsToQueryParams [toQueryParam tokenReq]
      clientSecretPostParam =
        if getClientAuthenticationMethod application == ClientSecretPost
          then clientSecretPost key
          else []

  if getClientAuthenticationMethod application == ClientAssertionJwt
    then do
      resp <- ExceptT . liftIO $ do
        req <- uriToRequest (idpTokenEndpoint idp)
        let req' = urlEncodedBody body (addDefaultRequestHeaders req)
        handleOAuth2TokenResponse <$> httpLbs req' mgr
      case parseResponseFlexible resp of
        Right obj -> return obj
        Left e -> throwE e
    else doJSONPostRequest mgr key (idpTokenEndpoint idp) (body ++ clientSecretPostParam)

-------------------------------------------------------------------------------
--                                    PKCE                                   --
-------------------------------------------------------------------------------

-- | Make Token Request (PKCE)
-- https://datatracker.ietf.org/doc/html/rfc7636#section-4.5
conduitPkceTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  (ExchangeTokenInfo a, CodeVerifier) ->
  ExceptT TokenResponseError m OAuth2Token
conduitPkceTokenRequest IdpApplication {..} mgr (exchangeToken, codeVerifier) =
  let req = mkTokenRequestParam application exchangeToken
      key = mkOAuth2Key application
      clientSecretPostParam =
        if getClientAuthenticationMethod application == ClientSecretPost
          then clientSecretPost key
          else []
      body =
        unionMapsToQueryParams
          [ toQueryParam req
          , toQueryParam codeVerifier
          ]
          ++ clientSecretPostParam
   in doJSONPostRequest mgr key (idpTokenEndpoint idp) body
