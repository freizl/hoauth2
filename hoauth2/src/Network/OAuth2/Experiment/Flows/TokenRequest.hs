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

class HasOAuth2Key a where
  mkOAuth2Key :: a -> OAuth2

class HasTokenRequestClientAuthenticationMethod a where
  getClientAuthenticationMethod :: a -> ClientAuthenticationMethod

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

conduitTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  ExchangeTokenInfo a ->
  ExceptT TokenRequestError m OAuth2Token
conduitTokenRequest IdpApplication {..} mgr exchangeToken = do
  let tokenReq = mkTokenRequestParam application exchangeToken
      body = mapsToParams [toQueryParam tokenReq]
  if getClientAuthenticationMethod application == ClientAssertionJwt
    then do
      resp <- ExceptT . liftIO $ do
        req <- uriToRequest (idpTokenEndpoint idp)
        let req' = urlEncodedBody body (addDefaultRequestHeaders req)
        handleOAuth2TokenResponse <$> httpLbs req' mgr
      case parseResponseFlexible resp of
        Right obj -> return obj
        Left e -> throwE e
    else doJSONPostRequest mgr (mkOAuth2Key application) (idpTokenEndpoint idp) body

-------------------------------------------------------------------------------
--                                    PKCE                                   --
-------------------------------------------------------------------------------

conduitPkceTokenRequest ::
  (HasTokenRequest a, ToQueryParam (TokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  (ExchangeTokenInfo a, CodeVerifier) ->
  ExceptT TokenRequestError m OAuth2Token
conduitPkceTokenRequest IdpApplication {..} mgr (exchangeToken, codeVerifier) =
  let req = mkTokenRequestParam application exchangeToken
      key = mkOAuth2Key application
      clientSecretPostParam =
        if getClientAuthenticationMethod application == ClientSecretPost
          then clientSecretPost key
          else []
      body =
        mapsToParams
          [ toQueryParam req
          , toQueryParam codeVerifier
          ]
          ++ clientSecretPostParam
   in doJSONPostRequest mgr key (idpTokenEndpoint idp) body
