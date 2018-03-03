{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE OverloadedStrings #-}

module IDP.Google where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Text.Lazy                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Hashable
import Keys
import Utils
import           Types

data Google = Google deriving (Show, Generic)

instance Hashable Google

instance IDP Google

instance HasLabel Google

instance HasTokenReq Google where
  tokenReq _ mgr code = fetchAccessToken mgr googleKey code

instance HasUserReq Google where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Google where
  authUri _ = createCodeUri googleKey [ ("state", "Google.test-state-123")
                                        , ("scope", "user_about_me,email")
                                        ] 

data GoogleUser = GoogleUser { name :: Text
                             , id   :: Text
                             } deriving (Show, Generic)

instance FromJSON GoogleUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]

toLoginUser :: GoogleUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }

getUserInfo :: FromJSON a => Manager -> AccessToken -> IO (OAuth2Result a LoginUser)
getUserInfo mgr at = do
  re <- authGetJSON mgr at userInfoUri
  return (second toLoginUser re)

getAccessToken :: Manager
               -> OAuth2
               -> ExchangeToken
               -> IO (OAuth2Result TR.Errors OAuth2Token)
getAccessToken = fetchAccessToken
