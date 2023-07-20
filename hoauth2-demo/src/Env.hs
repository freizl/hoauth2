module Env where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Maybe
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import GHC.Generics
import Network.OAuth2.Experiment
import System.Directory

newtype AppEnv = AppEnv (Aeson.KeyMap OAuthAppSettings)
  deriving (Generic)

instance FromJSON AppEnv

data OAuthAppSettings = OAuthAppSettings
  { clientId :: ClientId
  , clientSecret :: ClientSecret
  , scopes :: Set.Set Scope
  , user :: Maybe UserConfig
  }

instance FromJSON OAuthAppSettings where
  parseJSON = withObject "parseJSON OAuthAppSettings" $ \t -> do
    clientId <- ClientId <$> t .: "client_id"
    clientSecret <- ClientSecret <$> t .: "client_secret"
    scopeTexts <- t .:? "scopes"
    user <- t .:? "user"
    let scopes = Set.map Scope (Set.fromList (fromMaybe [] scopeTexts))
    pure OAuthAppSettings {..}

data UserConfig = UserConfig
  { username :: Text
  , password :: Text
  }
  deriving (Generic)

instance FromJSON UserConfig

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: MonadIO m => ExceptT Text m AppEnv
readEnvFile = do
  withExceptT wrapError $
    ExceptT $
      liftIO $ do
        pwd <- liftIO getCurrentDirectory
        eitherDecodeFileStrict (pwd <> "/" <> envFilePath)
  where
    wrapError :: String -> Text
    wrapError = TL.pack . ("Error when try to load .env.json\n" <>)

lookup :: MonadIO m => Text -> ExceptT Text m OAuthAppSettings
lookup idpAppName = do
  (AppEnv val) <- readEnvFile
  let key = Aeson.fromString $ TL.unpack $ TL.toLower idpAppName
      resp = Aeson.lookup key val
  except $
    maybe
      (Left $ "[ loadCredentialFromConfig2 ] unable to load config for " <> idpAppName)
      Right
      resp
