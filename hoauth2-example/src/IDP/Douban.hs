{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Douban where
import           Data.Aeson
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

newtype Douban = Douban OAuth2 deriving (Generic, Eq, Show)

instance Hashable Douban

instance IDP Douban

instance HasLabel Douban where
  idpLabel = const "Douban"

instance HasTokenReq Douban where
  tokenReq (Douban key) mgr = fetchAccessTokenInternal ClientSecretPost mgr key

instance HasTokenRefreshReq Douban where
  tokenRefreshReq (Douban key) mgr = refreshAccessTokenInternal ClientSecretPost mgr key

instance HasUserReq Douban where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri Douban where
  authUri (Douban key) = createCodeUri key [ ("state", "Douban.test-state-123")
                                        ]

data DoubanUser = DoubanUser { name :: Text
                             , uid  :: Text
                             } deriving (Show, Generic)

instance FromJSON DoubanUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.douban.com/v2/user/~me|]

toLoginUser :: DoubanUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }

