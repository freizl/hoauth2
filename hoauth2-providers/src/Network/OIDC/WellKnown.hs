{-# LANGUAGE DuplicateRecordFields #-}

module Network.OIDC.WellKnown where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import URI.ByteString

-- | Slim OpenID Configuration
-- TODO: could add more fields to be complete.
data OpenIDConfiguration = OpenIDConfiguration
  { issuer :: Text
  , authorizationEndpoint :: Text
  , tokenEndpoint :: Text
  , userinfoEndpoint :: Text
  , jwksUri :: Text
  }
  deriving (Generic)

data OpenIDConfigurationUris = OpenIDConfigurationUris
  { authorizationUri :: URI
  , tokenUri :: URI
  , userinfoUri :: URI
  , jwksUri :: URI
  }
  deriving (Generic)

instance FromJSON OpenIDConfiguration where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

wellknownUrl :: TL.Text
wellknownUrl = "/.well-known/openid-configuration"

fetchWellKnown ::
  (MonadIO m) =>
  -- | Domain
  TL.Text ->
  ExceptT Text m OpenIDConfiguration
fetchWellKnown domain = ExceptT $ do
  let uri = "https://" <> domain <> wellknownUrl
  req <- liftIO $ parseRequest (TL.unpack uri)
  resp <- httpLbs req
  return (handleWellKnownResponse resp)

fetchWellKnownUris :: (MonadIO m) => TL.Text -> ExceptT Text m OpenIDConfigurationUris
fetchWellKnownUris domain = do
  OpenIDConfiguration {..} <- fetchWellKnown domain
  withExceptT (TL.pack . show) $ do
    ae <- ExceptT $ pure (parseURI strictURIParserOptions $ BSL.toStrict $ TL.encodeUtf8 authorizationEndpoint)
    te <- ExceptT $ pure (parseURI strictURIParserOptions $ BSL.toStrict $ TL.encodeUtf8 tokenEndpoint)
    ue <- ExceptT $ pure (parseURI strictURIParserOptions $ BSL.toStrict $ TL.encodeUtf8 userinfoEndpoint)
    jwks <- ExceptT $ pure (parseURI strictURIParserOptions $ BSL.toStrict $ TL.encodeUtf8 jwksUri)
    pure
      OpenIDConfigurationUris
        { authorizationUri = ae
        , tokenUri = te
        , userinfoUri = ue
        , jwksUri = jwks
        }

handleWellKnownResponse :: Response ByteString -> Either Text OpenIDConfiguration
handleWellKnownResponse resp = do
  let rawBody = getResponseBody resp
  let rStatus = getResponseStatus resp
  if rStatus == status200
    then first (("handleWellKnownResponse decode response failed: " <>) . TL.pack) (eitherDecode rawBody)
    else Left $ "handleWellKnownResponse failed: " <> TL.pack (show rStatus) <> TL.decodeUtf8 rawBody
