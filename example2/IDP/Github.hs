{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE OverloadedStrings #-}

module IDP.Github where
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
import           Types
import qualified Data.Text.Lazy       as TL

data Github = Github deriving (Show, Generic)

instance Hashable Github

instance IDP Github

instance HasLabel Github

instance HasTokenReq Github where
  tokenReq _ mgr code = fetchAccessToken mgr githubKey code

instance HasUserReq Github where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Github where
  authUri _ = createCodeUri githubKey [("state", "Github.test-state-123")]

data GithubUser = GithubUser { name :: Text
                             , id   :: Integer
                             } deriving (Show, Generic)

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]

toLoginUser :: GithubUser -> LoginUser
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
