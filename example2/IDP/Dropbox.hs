{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE OverloadedStrings #-}

module IDP.Dropbox where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Text.Lazy                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Hashable
import Keys
import Utils

data Dropbox = Dropbox deriving (Show, Generic)

instance Hashable Dropbox

instance IDP Dropbox

instance HasLabel Dropbox

instance HasTokenReq Dropbox where
  tokenReq _ mgr code = fetchAccessToken mgr dropboxKey code

instance HasUserReq Dropbox where
  userReq _ mgr at = do
    re <- parseResponseJSON <$> authPostBS3 mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Dropbox where
  authUri _ = createCodeUri dropboxKey [ ("state", "Dropbox.test-state-123")
                                        , ("scope", "user_about_me,email")
                                        ] 

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
getAccessToken = fetchAccessToken
