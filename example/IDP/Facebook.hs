{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Facebook where
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

data Facebook = Facebook deriving (Show, Generic)

instance Hashable Facebook

instance IDP Facebook

instance HasLabel Facebook

instance HasTokenReq Facebook where
  tokenReq _ mgr = fetchAccessToken2 mgr facebookKey

instance HasTokenRefreshReq Facebook where
  tokenRefreshReq _ mgr rt = refreshAccessToken mgr facebookKey rt

instance HasUserReq Facebook where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Facebook where
  authUri _ = createCodeUri facebookKey [ ("state", "Facebook.test-state-123")
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
