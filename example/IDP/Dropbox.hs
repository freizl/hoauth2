{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

module IDP.Dropbox where
import           Data.Aeson
import           Data.Aeson.Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Text.Lazy                       (Text)
import           GHC.Generics
import Types

data DropboxName = DropboxName { displayName :: Text }
                 deriving (Show, Generic)
data DropboxUser = DropboxUser { email :: Text
                               , name :: DropboxName
                               } deriving (Show, Generic)

instance FromJSON DropboxName where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON DropboxUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.dropboxapi.com/2/users/get_current_account|]

toLoginUser :: DropboxUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = (displayName $ name ouser) }
