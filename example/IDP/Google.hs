{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module IDP.Google where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import           GHC.Generics
import           URI.ByteString
import           URI.ByteString.QQ

import           Types

data GoogleUser = GoogleUser { name :: Text
                             , id   :: Text
                             } deriving (Show, Generic)

instance FromJSON GoogleUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]

toLoginUser :: GoogleUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }
