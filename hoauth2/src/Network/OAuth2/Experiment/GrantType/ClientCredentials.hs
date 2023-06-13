{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Network.OAuth2.Experiment.GrantType.ClientCredentials where

import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..), ExchangeToken, OAuth2)
import Network.OAuth2.Experiment.CoreTypes
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Utils

data Application = Application
  { ccClientId :: ClientId
  , ccClientSecret :: ClientSecret
  , ccName :: Text
  , ccScope :: Set Scope
  , ccTokenRequestExtraParams :: Map Text Text
  , ccTokenRequestAuthenticationMethod :: ClientAuthenticationMethod
  }

instance HasOAuth2Key Application where
  mkOAuth2Key :: Application -> OAuth2
  mkOAuth2Key Application {..} = toOAuth2Key ccClientId ccClientSecret

instance HasTokenRequestClientAuthenticationMethod Application where
  getClientAuthenticationMethod :: Application -> ClientAuthenticationMethod
  getClientAuthenticationMethod Application {..} = ccTokenRequestAuthenticationMethod

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.4.2
instance HasTokenRequest Application where
  data TokenRequest Application = ClientCredentialsTokenRequest
    { trScope :: Set Scope
    , trGrantType :: GrantTypeValue
    , trClientAssertionType :: Text
    , trClientAssertion :: BS.ByteString
    , trExtraParams :: Map Text Text
    , trClientAuthenticationMethod :: ClientAuthenticationMethod
    }

  mkTokenRequestParam :: Application -> Maybe ExchangeToken -> TokenRequest Application
  mkTokenRequestParam Application {..} _ =
    ClientCredentialsTokenRequest
      { trScope = ccScope
      , trGrantType = GTClientCredentials
      , trClientAssertionType = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"
      , trClientAssertion = tlToBS $ unClientSecret ccClientSecret
      , trClientAuthenticationMethod = ccTokenRequestAuthenticationMethod
      , trExtraParams = ccTokenRequestExtraParams
      }

instance ToQueryParam (TokenRequest Application) where
  toQueryParam :: TokenRequest Application -> Map Text Text
  toQueryParam ClientCredentialsTokenRequest {..} =
    let clientAssertion =
          if trClientAuthenticationMethod == ClientAssertionJwt
            then
              Map.fromList
                [ ("client_assertion_type", trClientAssertionType)
                , ("client_assertion", bs8ToLazyText trClientAssertion)
                ]
            else Map.empty
     in Map.unions
          [ toQueryParam trGrantType
          , toQueryParam trScope
          , trExtraParams
          , clientAssertion
          ]
