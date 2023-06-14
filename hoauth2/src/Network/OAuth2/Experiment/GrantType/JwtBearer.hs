{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.GrantType.JwtBearer where

import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..), ExchangeToken, OAuth2)
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Utils

data Application = Application
  { jbName :: Text
  , jbJwtAssertion :: BS.ByteString
  }

-- JwtBearner doesn't use client_id and client_secret for authentication.
-- TODO: The ideal solution shall be do not implement `HasOAuth2Key`
-- but it will stop to re-use the method 'conduitTokenRequest' for JwtBearer flow.
instance HasOAuth2Key Application where
  mkOAuth2Key :: Application -> OAuth2
  mkOAuth2Key _ = def

instance HasTokenRequestClientAuthenticationMethod Application where
  getClientAuthenticationMethod :: Application -> ClientAuthenticationMethod
  getClientAuthenticationMethod _ = ClientAssertionJwt

instance HasTokenRequest Application where
  data TokenRequest Application = JwtBearerTokenRequest
    { trGrantType :: GrantTypeValue -- \| 'GTJwtBearer'
    , trAssertion :: BS.ByteString -- \| The the signed JWT token
    }

  mkTokenRequestParam :: Application -> Maybe ExchangeToken -> TokenRequest Application
  mkTokenRequestParam Application {..} _ =
    JwtBearerTokenRequest
      { trGrantType = GTJwtBearer
      , trAssertion = jbJwtAssertion
      }

instance ToQueryParam (TokenRequest Application) where
  toQueryParam :: TokenRequest Application -> Map Text Text
  toQueryParam JwtBearerTokenRequest {..} =
    Map.unions
      [ toQueryParam trGrantType
      , Map.singleton "assertion" (bs8ToLazyText trAssertion)
      ]

instance HasUserInfoRequest Application
