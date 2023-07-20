module Env where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import GHC.Generics
import System.Directory

data UserConfig = UserConfig
  { username :: Text
  , password :: Text
  }
  deriving (Generic)

data OAuthAppSettings = OAuthAppSettings
  { clientId :: Text
  , clientSecret :: Text
  , scopes :: Maybe [Text]
  , user :: Maybe UserConfig
  }
  deriving (Generic)

newtype AppEnv = AppEnv (Aeson.KeyMap OAuthAppSettings)
  deriving (Generic)

instance FromJSON OAuthAppSettings

instance FromJSON UserConfig

instance FromJSON AppEnv

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: (MonadIO m) => ExceptT Text m AppEnv
readEnvFile = do
  withExceptT wrapError $
    ExceptT $
      liftIO $ do
        pwd <- liftIO getCurrentDirectory
        eitherDecodeFileStrict (pwd <> "/" <> envFilePath)
  where
    wrapError :: String -> Text
    wrapError = TL.pack . ("Error when try to load .env.json\n" <>)

lookup :: (MonadIO m) => Text -> ExceptT Text m (Maybe OAuthAppSettings)
lookup idpAppName = do
  (AppEnv val) <- readEnvFile
  let key = Aeson.fromString $ TL.unpack $ TL.toLower idpAppName
  pure $ Aeson.lookup key val

