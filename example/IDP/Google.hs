{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Google where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy                    (Text)
import           GHC.Generics
import           Keys
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

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
                                      , ("scope", "https://www.googleapis.com/auth/userinfo.email")
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
