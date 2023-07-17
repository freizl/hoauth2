{-# LANGUAGE DerivingStrategies #-}

module Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson.Types
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.HTTP.Client.Contrib
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils
import URI.ByteString hiding (UserInfo)

-------------------------------------------------------------------------------
--                    Device Authorization Request                           --
-------------------------------------------------------------------------------
newtype DeviceCode = DeviceCode Text
  deriving newtype (FromJSON)

instance ToQueryParam DeviceCode where
  toQueryParam :: DeviceCode -> Map Text Text
  toQueryParam (DeviceCode dc) = Map.singleton "device_code" dc

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
  { arScope :: Set Scope
  , arClientId :: Maybe ClientId
  , arExtraParams :: Map Text Text
  }

instance ToQueryParam DeviceAuthorizationRequestParam where
  toQueryParam :: DeviceAuthorizationRequestParam -> Map Text Text
  toQueryParam DeviceAuthorizationRequestParam {..} =
    Map.unions
      [ toQueryParam arScope
      , toQueryParam arClientId
      , arExtraParams
      ]

-- | https://www.rfc-editor.org/rfc/rfc8628#section-3.1
class HasOAuth2Key a => HasDeviceAuthorizationRequest a where
  mkDeviceAuthorizationRequestParam :: a -> DeviceAuthorizationRequestParam

-- TODO: there is only (possibly always only) on instance of 'HasDeviceAuthorizationRequest'
-- Maybe consider to hard-code the data type instead of use type class.
conduitDeviceAuthorizationRequest ::
  (MonadIO m, HasDeviceAuthorizationRequest a) =>
  IdpApplication i a ->
  Manager ->
  ExceptT BSL.ByteString m DeviceAuthorizationResponse
conduitDeviceAuthorizationRequest IdpApplication {..} mgr = do
  case idpDeviceAuthorizationEndpoint idp of
    Nothing -> throwE "[conduiteDeviceAuthorizationRequest] Device Authorization Flow is not supported due to miss device_authorization_endpoint."
    Just deviceAuthEndpoint -> do
      let deviceAuthReq = mkDeviceAuthorizationRequestParam application
          oauth2Key = mkOAuth2Key application
          body = unionMapsToQueryParams [toQueryParam deviceAuthReq]
      ExceptT . liftIO $ do
        req <- addDefaultRequestHeaders <$> uriToRequest deviceAuthEndpoint
        -- Hacky:
        -- Missing clientId implies ClientSecretBasic authentication method.
        -- See Grant/DeviceAuthorization.hs
        let req' = case arClientId deviceAuthReq of
              Nothing -> addBasicAuth oauth2Key req
              Just _ -> req
        resp <- httpLbs (urlEncodedBody body req') mgr
        pure $ first ("[conduiteDeviceAuthorizationRequest] " <>) $ handleResponseJSON resp
