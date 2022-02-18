{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Github where
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils


data Github = Github OAuth2 deriving (Show, Generic, Eq)

instance Hashable Github

instance IDP Github

instance HasLabel Github where
  idpLabel = const "Github"

instance HasTokenReq Github where
  tokenReq (Github key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Github where
  tokenRefreshReq (Github key) mgr = refreshAccessToken mgr key

instance HasUserReq Github where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Github where
  authUri (Github key) = createCodeUri key [("state", "Github.test-state-123")]

data GithubUser = GithubUser { name :: Text
                             , id   :: Integer
                             } deriving (Show, Generic)

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }
