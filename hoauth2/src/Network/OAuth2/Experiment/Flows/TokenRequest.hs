{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.TokenRequest where

import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 (
  ClientAuthenticationMethod (..),
 )
import Network.OAuth2.Experiment.Types (
  ClientId (unClientId),
  ClientSecret (unClientSecret),
 )

addSecretToHeader :: ClientId -> ClientSecret -> Request -> Request
addSecretToHeader cid csecret =
  applyBasicAuth
    (T.encodeUtf8 $ TL.toStrict $ unClientId cid)
    (T.encodeUtf8 $ TL.toStrict $ unClientSecret csecret)

class HasTokenRequestClientAuthenticationMethod a where
  getClientAuthenticationMethod :: a -> ClientAuthenticationMethod
  addClientAuthToHeader :: a -> Request -> Request
  addClientAuthToHeader _ = id

-- | Only Authorization Code Grant involves a Exchange Token (Authorization Code).
-- ResourceOwnerPassword and Client Credentials make token request directly.
data NoNeedExchangeToken = NoNeedExchangeToken

class HasTokenRequestClientAuthenticationMethod a => HasTokenRequest a where
  -- Each GrantTypeFlow has slightly different request parameter to /token endpoint.
  data TokenRequest a
  type ExchangeTokenInfo a

  -- | Only 'AuthorizationCode flow (but not resource owner password nor client credentials) will use 'ExchangeToken' in the token request
  -- create type family to be explicit on it.
  -- with 'type instance WithExchangeToken a b = b' implies no exchange token
  -- v.s. 'type instance WithExchangeToken a b = ExchangeToken -> b' implies needing an exchange token
  -- type WithExchangeToken a b
  mkTokenRequestParam :: a -> ExchangeTokenInfo a -> TokenRequest a
