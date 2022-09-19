{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import Data.Aeson
import Data.Aeson.KeyMap
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics

data EnvConfigAuthParams = EnvConfigAuthParams
  { clientId :: Text,
    clientSecret :: Text,
    scopes :: Maybe [Text]
  }
  deriving (Generic)

instance Default EnvConfigAuthParams where
  def =
    EnvConfigAuthParams
      { clientId = "",
        clientSecret = "",
        scopes = Just []
      }

instance FromJSON EnvConfigAuthParams

type EnvConfig = KeyMap EnvConfigAuthParams
