{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Github API: http://developer.github.com/v3/oauth/

module Main where

import           Control.Monad        (mzero)
import           Data.Aeson
import qualified Data.ByteString      as BS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Network.HTTP.Conduit
import           URI.ByteString
import           URI.ByteString.QQ

import           Network.OAuth.OAuth2

import           Keys


main :: IO ()
main = do
    let state = "testGithubApi"
    mgr <- newManager tlsManagerSettings
    putStrLn "Trying invalid token..."
    failToken <- getToken state "invalidCode" mgr
    print $ (failToken :: OAuth2Result OAuth2Token)
    print $ serializeURIRef' $ appendQueryParams [("state", state)] $ authorizationUrl githubKey
    putStrLn "visit the url and paste code here: "
    code <- getLine
    token <- getToken state code mgr
    print (token :: OAuth2Result OAuth2Token)
    case token of
      Right at -> userInfo mgr (accessToken at) >>= print
      Left _   -> putStrLn "no access token found yet"

getToken :: FromJSON a => BS.ByteString -> String -> Manager -> IO (OAuth2Result a)
getToken state code mgr = do
    let (url, body) = accessTokenUrl githubKey $ ExchangeToken $ T.pack code
    doJSONPostRequest mgr githubKey url (body ++ [("state", state)])

-- | Test API: user
--
userInfo :: Manager -> AccessToken -> IO (OAuth2Result GithubUser)
userInfo mgr token = authGetJSON mgr token [uri|https://api.github.com/user|]

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
