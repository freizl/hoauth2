{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Google where
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Keys
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

data Google = Google deriving (Show, Generic, Eq)

instance Hashable Google

instance IDP Google

instance HasLabel Google

instance HasTokenReq Google where
  tokenReq _ mgr = fetchAccessToken mgr googleKey

instance HasTokenRefreshReq Google where
  tokenRefreshReq _ mgr = refreshAccessToken mgr googleKey

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

