{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Github API: http://developer.github.com/v3/oauth/

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit (MonadResource)

import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2

import Github.Key

githuboauth :: OAuth2
githuboauth = githubKey { oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                        , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                        , oauthAccessToken = Nothing
                        }
 
main :: IO ()
main = do
    let state = "testGithubApi"
    print $ (authorizationUrl githuboauth) `appendQueryParam` [("state", state)]
    putStrLn "visit the url and paste code here: "
    code <- getLine
    let (url, body) = accessTokenUrl githuboauth (sToBS code)
    token <- doJSONPostRequest (url, body ++ [("state", state)])
    print (token :: Maybe AccessToken)

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
