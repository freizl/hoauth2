{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module IDP.Dropbox where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Bifunctor
import           Network.OAuth.OAuth2
import           Network.HTTP.Conduit
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import TokenUtil

newtype DropboxName = DropboxName { displayName :: Text }
                 deriving (Show, Generic)

data DropboxUser = DropboxUser { email :: Text
                               , name  :: DropboxName
                               } deriving (Show, Generic)

instance FromJSON DropboxName where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON DropboxUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.dropboxapi.com/2/users/get_current_account|]

toLoginUser :: DropboxUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = displayName $ name ouser }

getUserInfo :: FromJSON a => Manager -> AccessToken -> IO (OAuth2Result a LoginUser)
getUserInfo mgr at = do
  re <- parseResponseJSON <$> authPostBS3 mgr at userInfoUri
  return (second toLoginUser re)

getAccessToken :: Manager
               -> OAuth2
               -> ExchangeToken
               -> IO (OAuth2Result TR.Errors OAuth2Token)
getAccessToken = getAT
