{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.Grants.JwtBearer where

import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..), OAuth2)
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

-- JwtBearner doesn't use @client_id@ and @client_secret@ for authentication.
--
-- FIXME: The ideal solution shall be do not implement `HasOAuth2Key`
-- but it will stop to re-use the method 'conduitTokenRequest' for JwtBearer flow.
-- `HasTokenRequest` could implement another method `updateHeader :: a -> RequestHeader -> RequestHeader`
-- default is `id`
instance HasOAuth2Key JwtBearerApplication where
  mkOAuth2Key :: JwtBearerApplication -> OAuth2
  mkOAuth2Key _ = def

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
