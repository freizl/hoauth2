{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Google where
import           Data.Aeson
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

newtype Google = Google OAuth2 deriving (Show, Generic, Eq)

instance Hashable Google

instance IDP Google

instance HasLabel Google where
  idpLabel = const "Google"

instance HasTokenReq Google where
  tokenReq (Google key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Google where
  tokenRefreshReq (Google key) mgr = refreshAccessToken mgr key

instance HasUserReq Google where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri Google where
  authUri (Google key) = createCodeUri key [ ("state", "Google.test-state-123")
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

