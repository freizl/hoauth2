{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.TokenRequest where

import Network.OAuth.OAuth2 (
  ClientAuthenticationMethod (..),
 )
import Network.OAuth2.Experiment.Types

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
