{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.Grants.JwtBearer where

import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..))
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils

-- | An Application that supports "JWT Bearer" flow
--
-- https://datatracker.ietf.org/doc/html/rfc7523
data JwtBearerApplication = JwtBearerApplication
  { jbName :: Text
  , jbJwtAssertion :: BS.ByteString
  }

instance HasTokenRequestClientAuthenticationMethod JwtBearerApplication where
  getClientAuthenticationMethod :: JwtBearerApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod _ = ClientAssertionJwt

instance HasTokenRequest JwtBearerApplication where
  type ExchangeTokenInfo JwtBearerApplication = NoNeedExchangeToken

  data TokenRequest JwtBearerApplication = JwtBearerTokenRequest
    { trGrantType :: GrantTypeValue -- \| 'GTJwtBearer'
    , trAssertion :: BS.ByteString -- \| The the signed JWT token
    }

  mkTokenRequestParam :: JwtBearerApplication -> NoNeedExchangeToken -> TokenRequest JwtBearerApplication
  mkTokenRequestParam JwtBearerApplication {..} _ =
    JwtBearerTokenRequest
      { trGrantType = GTJwtBearer
      , trAssertion = jbJwtAssertion
      }

instance ToQueryParam (TokenRequest JwtBearerApplication) where
  toQueryParam :: TokenRequest JwtBearerApplication -> Map Text Text
  toQueryParam JwtBearerTokenRequest {..} =
    Map.unions
      [ toQueryParam trGrantType
      , Map.singleton "assertion" (bs8ToLazyText trAssertion)
      ]

instance HasUserInfoRequest JwtBearerApplication
