{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Github API: http://developer.github.com/v3/oauth/

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust)
import qualified Data.Text as T
import  Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit (MonadResource)
import           Control.Applicative
import           Control.Monad                     (mzero)
import           Data.Aeson

import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2

import Keys


main :: IO ()
main = do
    let state = "testGithubApi"
    print $ (authorizationUrl githubKey) `appendQueryParam` [("state", state)]
    putStrLn "visit the url and paste code here: "
    code <- getLine
    let (url, body) = accessTokenUrl githubKey (sToBS code)
    token <- doJSONPostRequest (url, body ++ [("state", state)])
    print (token :: Maybe AccessToken)
    case token of
      Just (AccessToken t _) -> userInfo (githubKey {oauthAccessToken = Just t}) >>= print
      _      -> print "no access token found yet"

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack

-- | Test API: user
--
userInfo :: OAuth2 -> IO (Maybe GithubUser)
userInfo oauth = doJSONGetRequest (appendAccessToken
                                     "https://api.github.com/user"
                                     oauth)

data GithubUser = GithubUser { gid    :: Integer
                             , gname  :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "id"
                           <*> o .: "name"
    parseJSON _ = mzero
