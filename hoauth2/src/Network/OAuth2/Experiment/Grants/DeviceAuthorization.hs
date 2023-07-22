{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.Grants.DeviceAuthorization (
  DeviceAuthorizationApplication (..),
  pollDeviceTokenRequest,
) where

import Control.Concurrent
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.HTTP.Conduit
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
  , daAuthorizationRequestAuthenticationMethod :: Maybe ClientAuthenticationMethod
  -- ^ The spec requires similar authentication method as /token request.
  -- Most of identity providers doesn't required it but some does like Okta.
  }

pollDeviceTokenRequest ::
  MonadIO m =>
  IdpApplication i DeviceAuthorizationApplication ->
  Manager ->
  DeviceAuthorizationResponse ->
  ExceptT TokenRequestError m OAuth2Token
pollDeviceTokenRequest idpApp mgr deviceAuthResp = do
  pollDeviceTokenRequestInternal
    idpApp
    mgr
    (deviceCode deviceAuthResp)
    (fromMaybe 5 $ interval deviceAuthResp)

pollDeviceTokenRequestInternal ::
  MonadIO m =>
  IdpApplication i DeviceAuthorizationApplication ->
  Manager ->
  DeviceCode ->
  Int ->
  -- | Polling Interval
  ExceptT TokenRequestError m OAuth2Token
pollDeviceTokenRequestInternal idpApp mgr deviceCode intervalSeconds = do
  resp <- runExceptT (conduitTokenRequest idpApp mgr deviceCode)
  case resp of
    Left trRespError -> do
      case error trRespError of
        -- TODO: Didn't have a good idea to expand the error code
        -- specifically for device token request flow
        -- Device Token Response additional error code: https://www.rfc-editor.org/rfc/rfc8628#section-3.5
        UnknownErrorCode "authorization_pending" -> do
          liftIO $ threadDelay $ intervalSeconds * 1000000
          pollDeviceTokenRequestInternal idpApp mgr deviceCode intervalSeconds
        UnknownErrorCode "slow_down" -> do
          let newIntervalSeconds = intervalSeconds + 5
          liftIO $ threadDelay $ newIntervalSeconds * 1000000
          pollDeviceTokenRequestInternal idpApp mgr deviceCode newIntervalSeconds
        _ -> throwE trRespError
    Right v -> pure v

instance HasOAuth2Key DeviceAuthorizationApplication where
  mkOAuth2Key :: DeviceAuthorizationApplication -> OAuth2
  mkOAuth2Key DeviceAuthorizationApplication {..} = toOAuth2Key daClientId daClientSecret

instance HasTokenRequestClientAuthenticationMethod DeviceAuthorizationApplication where
  getClientAuthenticationMethod :: DeviceAuthorizationApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod _ = ClientSecretBasic

instance HasDeviceAuthorizationRequest DeviceAuthorizationApplication where
  mkDeviceAuthorizationRequestParam :: DeviceAuthorizationApplication -> DeviceAuthorizationRequestParam
  mkDeviceAuthorizationRequestParam DeviceAuthorizationApplication {..} =
    DeviceAuthorizationRequestParam
      { arScope = daScope
      , arClientId =
          if daAuthorizationRequestAuthenticationMethod == Just ClientSecretBasic
            then Nothing
            else Just daClientId
      , arExtraParams = daAuthorizationRequestExtraParam
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
    -- The token request use 'ClientSecretBasic' by default. (has to pick up one Client Authn Method)
    -- ClientId shall be also be in request body per spec.
    -- However, for some IdPs, e.g. Okta, when using 'ClientSecretBasic' to authn Client,
    -- it doesn't allow 'client_id' in the request body
    -- 'daAuthorizationRequestAuthenticationMethod' set the tone for Authorization Request,
    -- hence just follow it in the token request
    AuthorizationCodeTokenRequest
      { trCode = deviceCode
      , trGrantType = GTDeviceCode
      , trClientId =
          if daAuthorizationRequestAuthenticationMethod == Just ClientSecretBasic
            then Nothing
            else Just daClientId
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
