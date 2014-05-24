{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Github API: http://developer.github.com/v3/oauth/

module Main where

import           Control.Applicative
import           Control.Monad                   (mzero)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Aeson
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Conduit                    (MonadResource)
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types              as HT

import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import           Keys


main :: IO ()
main = do
    let state = "testGithubApi"
    print $ authorizationUrl githubKey `appendQueryParam` [("state", state)]
    putStrLn "visit the url and paste code here: "
    code <- getLine
    let (url, body) = accessTokenUrl githubKey (sToBS code)
    token <- doJSONPostRequest githubKey url (body ++ [("state", state)])
    print (token :: OAuth2Result AccessToken)
    case token of
      Right at  -> userInfo at >>= print
      Left l    -> print "no access token found yet"


-- | Test API: user
--
userInfo :: AccessToken -> IO (OAuth2Result GithubUser)
userInfo token = authGetJSON token "https://api.github.com/user"

data GithubUser = GithubUser { gid   :: Integer
                             , gname :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "id"
                           <*> o .: "name"
    parseJSON _ = mzero

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
