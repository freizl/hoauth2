{-# LANGUAGE OverloadedStrings #-}

{-
google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer
-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)

import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
import Google.Key

gauth :: OAuth2
gauth = googleKeys { oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token" 
                   , oauthAccessToken = Nothing
                   }

main :: IO ()
main = do 
          print $ authorizationUrl gauth `BS.append` "&" `BS.append` googleScopeStr
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken at (Just rt))) <- requestAccessToken gauth (BS.pack code)
          print (at, rt)
          refreshAccessToken gauth rt >>= print


-- | this is special for google.
googleScopeStr = renderSimpleQuery False [("scope", "https://www.googleapis.com/auth/userinfo.email")]
