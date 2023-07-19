module Env where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics

data UserConfig = UserConfig
  { username :: Text
  , password :: Text
  }
  deriving (Generic)

data EnvConfigAuthParams = EnvConfigAuthParams
  { clientId :: Text
  , clientSecret :: Text
  , scopes :: Maybe [Text]
  , user :: Maybe UserConfig
  }
  deriving (Generic)

instance Default EnvConfigAuthParams where
  def =
    EnvConfigAuthParams
      { clientId = ""
      , clientSecret = ""
      , scopes = Nothing
      , user = Nothing
      }

instance FromJSON EnvConfigAuthParams

instance FromJSON UserConfig

type EnvConfig = KeyMap EnvConfigAuthParams
