{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.Grants.DeviceAuthorization where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Types
import Prelude hiding (error)

-- | An Application that supports "Device Authorization Grant"
--
-- https://www.rfc-editor.org/rfc/rfc8628#section-3.1
data DeviceAuthorizationApplication = DeviceAuthorizationApplication
  { daName :: Text
  , daClientId :: ClientId
  , daClientSecret :: ClientSecret
  , daScope :: Set Scope
  , daAuthorizationRequestExtraParam :: Map Text Text
  -- ^ Additional parameters to the device authorization request.
  -- Most of identity providers follow the spec strictly but
  -- AzureAD requires "tenant" parameter.
  , daAuthorizationRequestAuthenticationMethod :: ClientAuthenticationMethod
  -- ^ The spec requires similar authentication method as /token request.
  -- Most of identity providers doesn't required it but some does like Okta.
  }

instance HasClientAuthenticationMethod DeviceAuthorizationApplication where
  getClientAuthenticationMethod :: DeviceAuthorizationApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod = daAuthorizationRequestAuthenticationMethod
  addClientAuthToHeader DeviceAuthorizationApplication {..} = addSecretToHeader daClientId daClientSecret

mkDeviceAuthorizationRequestParam :: DeviceAuthorizationApplication -> DeviceAuthorizationRequestParam
mkDeviceAuthorizationRequestParam DeviceAuthorizationApplication {..} =
  DeviceAuthorizationRequestParam
    { darScope = daScope
    , darClientId =
        if daAuthorizationRequestAuthenticationMethod == ClientSecretPost
          then Just daClientId
          else Nothing
    , darExtraParams = daAuthorizationRequestExtraParam
    }

-- | https://www.rfc-editor.org/rfc/rfc8628#section-3.4
instance HasTokenRequest DeviceAuthorizationApplication where
  type ExchangeTokenInfo DeviceAuthorizationApplication = DeviceCode
  data TokenRequest DeviceAuthorizationApplication = AuthorizationCodeTokenRequest
    { trCode :: DeviceCode
    , trGrantType :: GrantTypeValue
    , trClientId :: Maybe ClientId
    }

  mkTokenRequestParam ::
    DeviceAuthorizationApplication ->
    DeviceCode ->
    TokenRequest DeviceAuthorizationApplication
  mkTokenRequestParam DeviceAuthorizationApplication {..} deviceCode =
    --
    -- This is a bit hacky!
    -- The token request use `ClientSecretBasic` by default. (has to pick up one Client Authn Method)
    -- ClientId shall be also be in request body per spec.
    -- However, for some IdPs, e.g. Okta, when using `ClientSecretBasic` to authn Client,
    -- it doesn't allow @client_id@ in the request body
    -- 'daAuthorizationRequestAuthenticationMethod' set the tone for Authorization Request,
    -- hence just follow it in the token request
    AuthorizationCodeTokenRequest
      { trCode = deviceCode
      , trGrantType = GTDeviceCode
      , trClientId =
          if daAuthorizationRequestAuthenticationMethod == ClientSecretPost
            then Just daClientId
            else Nothing
      }

instance ToQueryParam (TokenRequest DeviceAuthorizationApplication) where
  toQueryParam :: TokenRequest DeviceAuthorizationApplication -> Map Text Text
  toQueryParam AuthorizationCodeTokenRequest {..} =
    Map.unions
      [ toQueryParam trCode
      , toQueryParam trGrantType
      , toQueryParam trClientId
      ]

instance HasUserInfoRequest DeviceAuthorizationApplication
