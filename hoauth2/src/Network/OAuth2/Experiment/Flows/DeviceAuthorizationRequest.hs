{-# LANGUAGE DerivingStrategies #-}

module Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest where

import Control.Applicative
import Data.Aeson.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth2.Experiment.Types
import URI.ByteString hiding (UserInfo)

-------------------------------------------------------------------------------
--                    Device Authorization Request                           --
-------------------------------------------------------------------------------
newtype DeviceCode = DeviceCode Text
  deriving newtype (FromJSON)

instance ToQueryParam DeviceCode where
  toQueryParam :: DeviceCode -> Map Text Text
  toQueryParam (DeviceCode dc) = Map.singleton "device_code" dc

-- | https://www.rfc-editor.org/rfc/rfc8628#section-3.2
data DeviceAuthorizationResponse = DeviceAuthorizationResponse
  { deviceCode :: DeviceCode
  , userCode :: Text
  , verificationUri :: URI
  , verificationUriComplete :: Maybe URI
  , expiresIn :: Integer
  , interval :: Maybe Int
  }

instance FromJSON DeviceAuthorizationResponse where
  parseJSON :: Value -> Parser DeviceAuthorizationResponse
  parseJSON = withObject "parse DeviceAuthorizationResponse" $ \t -> do
    deviceCode <- t .: "device_code"
    userCode <- t .: "user_code"
    -- https://stackoverflow.com/questions/76696956/shall-it-be-verification-uri-instead-of-verification-url-in-the-device-autho
    verificationUri <- t .: "verification_uri" <|> t .: "verification_url"
    verificationUriComplete <- t .:? "verification_uri_complete"
    expiresIn <- t .: "expires_in"
    interval <- t .:? "interval"
    pure DeviceAuthorizationResponse {..}

data DeviceAuthorizationRequestParam = DeviceAuthorizationRequestParam
  { darScope :: Set Scope
  , darClientId :: Maybe ClientId
  , darExtraParams :: Map Text Text
  }

instance ToQueryParam DeviceAuthorizationRequestParam where
  toQueryParam :: DeviceAuthorizationRequestParam -> Map Text Text
  toQueryParam DeviceAuthorizationRequestParam {..} =
    Map.unions
      [ toQueryParam darScope
      , toQueryParam darClientId
      , darExtraParams
      ]
