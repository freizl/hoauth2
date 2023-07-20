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

newtype OAuthAppSettings = OAuthAppSettings (Aeson.KeyMap OAuthAppSetting)
  deriving (Generic)

instance FromJSON OAuthAppSettings

data OAuthAppSetting = OAuthAppSetting
  { clientId :: ClientId
  , clientSecret :: ClientSecret
  , scopes :: Set.Set Scope
  , user :: Maybe UserConfig
  }

instance FromJSON OAuthAppSetting where
  parseJSON = withObject "parseJSON OAuthAppSetting" $ \t -> do
    clientId <- ClientId <$> t .: "client_id"
    clientSecret <- ClientSecret <$> t .: "client_secret"
    scopeTexts <- t .:? "scopes"
    user <- t .:? "user"
    let scopes = Set.map Scope (Set.fromList (fromMaybe [] scopeTexts))
    pure OAuthAppSetting {..}

data UserConfig = UserConfig
  { username :: Text
  , password :: Text
  }
  deriving (Generic)

instance FromJSON UserConfig

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: MonadIO m => ExceptT Text m OAuthAppSettings
readEnvFile = do
  withExceptT wrapError $
    ExceptT $
      liftIO $ do
        pwd <- liftIO getCurrentDirectory
        eitherDecodeFileStrict (pwd <> "/" <> envFilePath)
  where
    wrapError :: String -> Text
    wrapError = TL.pack . ("Error when try to load .env.json\n" <>)

lookup :: MonadIO m => Text -> ExceptT Text m OAuthAppSetting
lookup idpAppName = do
  envs <- readEnvFile
  lookupWith envs idpAppName

lookupWith :: MonadIO m => OAuthAppSettings -> Text -> ExceptT Text m OAuthAppSetting
lookupWith (OAuthAppSettings val) idpAppName = do
  let key = Aeson.fromString $ TL.unpack $ TL.toLower idpAppName
      resp = Aeson.lookup key val
  except $
    maybe
      (Left $ "[ loadCredentialFromConfig2 ] unable to load config for " <> idpAppName)
      Right
      resp
