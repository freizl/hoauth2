{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Facebook where
import           Data.Aeson
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

newtype Facebook = Facebook OAuth2 deriving (Show, Generic, Eq)

instance Hashable Facebook

instance IDP Facebook

instance HasLabel Facebook where
  idpLabel = const "Facebook"

instance HasTokenReq Facebook where
  tokenReq (Facebook key) mgr = fetchAccessToken2 mgr key

instance HasTokenRefreshReq Facebook where
  tokenRefreshReq (Facebook key) mgr = refreshAccessToken mgr key

instance HasUserReq Facebook where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri Facebook where
  authUri (Facebook key) = createCodeUri key [ ("state", "Facebook.test-state-123")
                                        , ("scope", "user_about_me,email")
                                        ]

data FacebookUser = FacebookUser { id    :: Text
                                 , name  :: Text
                                 , email :: Text
                                 } deriving (Show, Generic)

instance FromJSON FacebookUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://graph.facebook.com/me?fields=id,name,email|]

toLoginUser :: FacebookUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }
