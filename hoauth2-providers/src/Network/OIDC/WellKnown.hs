{-# LANGUAGE CPP #-}

module Network.OIDC.WellKnown where

import Control.Monad.Except
#if MIN_VERSION_base(4,18,0)
import Control.Monad.IO.Class
#endif
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import URI.ByteString
import URI.ByteString.Aeson ()

-- | Slim OpenID Configuration
-- TODO: could add more fields to be complete.
--
-- See spec <https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata>
data OpenIDConfiguration = OpenIDConfiguration
  { issuer :: URI
  , authorizationEndpoint :: URI
  , tokenEndpoint :: URI
  , userinfoEndpoint :: URI
  , jwksUri :: URI
  , deviceAuthorizationEndpoint :: URI
  }
  deriving (Show, Eq)

instance FromJSON OpenIDConfiguration where
  parseJSON :: Value -> Parser OpenIDConfiguration
  parseJSON = withObject "parseJSON OpenIDConfiguration" $ \t -> do
    issuer <- t .: "issuer"
    authorizationEndpoint <- t .: "authorization_endpoint"
    tokenEndpoint <- t .: "token_endpoint"
    userinfoEndpoint <- t .: "userinfo_endpoint"
    jwksUri <- t .: "jwks_uri"
    deviceAuthorizationEndpoint <- t .: "device_authorization_endpoint"
    pure OpenIDConfiguration {..}

wellknownUrl :: TL.Text
wellknownUrl = "/.well-known/openid-configuration"

fetchWellKnown ::
  MonadIO m =>
  -- | Domain
  TL.Text ->
  ExceptT Text m OpenIDConfiguration
fetchWellKnown domain = ExceptT $ do
  let uri = "https://" <> domain <> wellknownUrl
  req <- liftIO $ parseRequest (TL.unpack uri)
  resp <- httpLbs req
  pure (handleWellKnownResponse resp)

handleWellKnownResponse :: Response ByteString -> Either Text OpenIDConfiguration
handleWellKnownResponse resp = do
  let rawBody = getResponseBody resp
  let rStatus = getResponseStatus resp
  if rStatus == status200
    then first (("handleWellKnownResponse decode response failed: " <>) . TL.pack) (eitherDecode rawBody)
    else Left $ "handleWellKnownResponse failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody
