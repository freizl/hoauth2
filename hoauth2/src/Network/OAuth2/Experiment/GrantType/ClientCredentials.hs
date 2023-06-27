{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.GrantType.ClientCredentials where

import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..), OAuth2)
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils

-- | An Application that supports "Client Credentials" flow
--
-- https://www.rfc-editor.org/rfc/rfc6749#section-4.4
data ClientCredentialsApplication = ClientCredentialsApplication
  { ccClientId :: ClientId
  , ccClientSecret :: ClientSecret
  , ccName :: Text
  , ccScope :: Set Scope
  , ccTokenRequestExtraParams :: Map Text Text
  , ccTokenRequestAuthenticationMethod :: ClientAuthenticationMethod
  }

instance HasOAuth2Key ClientCredentialsApplication where
  mkOAuth2Key :: ClientCredentialsApplication -> OAuth2
  mkOAuth2Key ClientCredentialsApplication {..} = toOAuth2Key ccClientId ccClientSecret

instance HasTokenRequestClientAuthenticationMethod ClientCredentialsApplication where
  getClientAuthenticationMethod :: ClientCredentialsApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod ClientCredentialsApplication {..} = ccTokenRequestAuthenticationMethod

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.4.2
instance HasTokenRequest ClientCredentialsApplication where
  type ExchangeTokenInfo ClientCredentialsApplication = ()
  data TokenRequest ClientCredentialsApplication = ClientCredentialsTokenRequest
    { trScope :: Set Scope
    , trGrantType :: GrantTypeValue
    , trClientAssertionType :: Text
    , trClientAssertion :: BS.ByteString
    , trExtraParams :: Map Text Text
    , trClientAuthenticationMethod :: ClientAuthenticationMethod
    }

  mkTokenRequestParam :: ClientCredentialsApplication -> () -> TokenRequest ClientCredentialsApplication
  mkTokenRequestParam ClientCredentialsApplication {..} _ =
    ClientCredentialsTokenRequest
      { trScope = ccScope
      , trGrantType = GTClientCredentials
      , trClientAssertionType = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"
      , trClientAssertion = tlToBS $ unClientSecret ccClientSecret
      , trClientAuthenticationMethod = ccTokenRequestAuthenticationMethod
      , trExtraParams = ccTokenRequestExtraParams
      }

instance ToQueryParam (TokenRequest ClientCredentialsApplication) where
  toQueryParam :: TokenRequest ClientCredentialsApplication -> Map Text Text
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
