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

data OAuthDomains = OAuthDomains
  { auth0Domain :: Text
  , oktaDomain :: Text
  }

instance FromJSON OAuthDomains where
  parseJSON = withObject "parseJSON OAuthDomains" $ \t -> do
    auth0Domain <- t .: "auth0"
    oktaDomain <- t .: "okta"
    pure OAuthDomains {..}

newtype OAuthApps = OAuthApps (Aeson.KeyMap OAuthApp)
  deriving (Generic)

unOAuthApps :: OAuthApps -> Aeson.KeyMap OAuthApp
unOAuthApps (OAuthApps x) = x

instance FromJSON OAuthApps

data OAuthApp = OAuthApp
  { clientId :: ClientId
  , clientSecret :: ClientSecret
  , scopes :: Set.Set Scope
  , user :: Maybe UserConfig
  }

instance FromJSON OAuthApp where
  parseJSON = withObject "parseJSON OAuthApp" $ \t -> do
    clientId <- ClientId <$> t .: "client_id"
    clientSecret <- ClientSecret <$> t .: "client_secret"
    scopeTexts <- t .:? "scopes"
    user <- t .:? "user"
    let scopes = Set.map Scope (Set.fromList (fromMaybe [] scopeTexts))
    pure OAuthApp {..}

data UserConfig = UserConfig
  { username :: Text
  , password :: Text
  }
  deriving (Generic)

instance FromJSON UserConfig

data OAuthConfigs = OAuthConfigs
  { domains :: OAuthDomains
  , apps :: OAuthApps
  }

instance FromJSON OAuthConfigs where
  parseJSON = withObject "parseJSON OAuthConfigs" $ \t -> do
    domains <- t .: "domains"
    apps <- t .: "apps"
    pure OAuthConfigs {..}

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: MonadIO m => ExceptT Text m OAuthConfigs
readEnvFile = do
  withExceptT wrapError $
    ExceptT $
      liftIO $ do
        pwd <- liftIO getCurrentDirectory
        eitherDecodeFileStrict (pwd <> "/" <> envFilePath)
  where
    wrapError :: String -> Text
    wrapError = TL.pack . ("Error when try to load .env.json\n" <>)

lookupApp :: MonadIO m => Text -> ExceptT Text m OAuthApp
lookupApp idpAppName = do
  envs <- readEnvFile
  lookupAppWith envs idpAppName

lookupAppWith :: MonadIO m => OAuthConfigs -> Text -> ExceptT Text m OAuthApp
lookupAppWith OAuthConfigs {..} idpAppName = do
  let key = TL.toLower idpAppName
      resp = Aeson.lookup (Aeson.fromString $ TL.unpack key) (unOAuthApps apps)
  except $
    maybe
      (Left $ "[ Env.lookupWith ] Unable to load config for " <> key)
      Right
      resp

lookupAuth0Domain :: MonadIO m => ExceptT Text m Text
lookupAuth0Domain =
  auth0Domain . domains <$> readEnvFile
