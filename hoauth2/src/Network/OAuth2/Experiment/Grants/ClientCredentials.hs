{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.Grants.ClientCredentials where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..))
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
  , ccClientAuthenticationMethod :: ClientAuthenticationMethod
  }

instance HasTokenRequestClientAuthenticationMethod ClientCredentialsApplication where
  getClientAuthenticationMethod :: ClientCredentialsApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod ClientCredentialsApplication {..} = ccClientAuthenticationMethod

  addClientAuthToHeader ClientCredentialsApplication {..} = addSecretToHeader ccClientId ccClientSecret

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.4.2
instance HasTokenRequest ClientCredentialsApplication where
  type ExchangeTokenInfo ClientCredentialsApplication = NoNeedExchangeToken
  data TokenRequest ClientCredentialsApplication = ClientCredentialsTokenRequest
    { trScope :: Set Scope
    , trGrantType :: GrantTypeValue
    , trClientSecret :: ClientSecret
    , trClientId :: ClientId
    , trExtraParams :: Map Text Text
    , trClientAuthenticationMethod :: ClientAuthenticationMethod
    }

  mkTokenRequestParam :: ClientCredentialsApplication -> NoNeedExchangeToken -> TokenRequest ClientCredentialsApplication
  mkTokenRequestParam ClientCredentialsApplication {..} _ =
    ClientCredentialsTokenRequest
      { trScope = ccScope
      , trGrantType = GTClientCredentials
      , trClientSecret = ccClientSecret
      , trClientAuthenticationMethod = ccClientAuthenticationMethod
      , trExtraParams = ccTokenRequestExtraParams
      , trClientId = ccClientId
      }

instance ToQueryParam (TokenRequest ClientCredentialsApplication) where
  toQueryParam :: TokenRequest ClientCredentialsApplication -> Map Text Text
  toQueryParam ClientCredentialsTokenRequest {..} =
    let jwtAssertionBody =
          if trClientAuthenticationMethod == ClientAssertionJwt
            then
              [ toQueryParam trClientId
              , Map.fromList
                  [ ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
                  , ("client_assertion", bs8ToLazyText $ tlToBS $ unClientSecret trClientSecret)
                  ]
              ]
            else []
     in Map.unions $
          [ toQueryParam trGrantType
          , toQueryParam trScope
          , trExtraParams
          ]
            ++ jwtAssertionBody
