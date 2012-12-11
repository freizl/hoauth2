{-# LANGUAGE OverloadedStrings #-}

{-

This is basically very manual test. Check following link for details.

google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import System.Environment

import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2
import Google.Key


--------------------------------------------------

main :: IO ()
main = do
    (x:_) <- getArgs
    case x of
        "normal" -> normalCase
        "offline" -> offlineCase


offlineCase :: IO ()
offlineCase = do 
          print $ authorizationUrl googleKeys `BS.append` "&" `BS.append` extraParams
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKeys (BS.pack code) 
          print (accessToken, refreshToken)
          validateToken accessToken >>= print
          -- 
          -- obtain a new access token with refresh token, which turns out only in response at first time.
          -- Revoke Access https://www.google.com/settings/security
          --
          case refreshToken of
            Nothing -> print "Failed to fetch refresh token"
            Just tk -> refreshAccessToken googleKeys tk >>= print
    where extraParams = renderSimpleQuery False $ ("access_type", "offline"):googleScopeEmail


normalCase :: IO ()
normalCase = do 
          print $ authorizationUrl googleKeys `BS.append` "&" `BS.append` extraParams
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken accessToken Nothing)) <- requestAccessToken googleKeys (BS.pack code) 
          print accessToken
          res <- validateToken accessToken
          print res
          res2 <- userinfo accessToken
          print res2
    where extraParams = renderSimpleQuery False googleScopeEmail


--------------------------------------------------
-- Google API

-- | this is special for google Gain read-only access to the user's email address.
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

-- | Gain read-only access to basic profile information, including a 
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- Token Validation
validateToken accessToken = doSimplePostRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo", 
                                                 (accessTokenToParam accessToken))

-- | fetch user email.
--   for more information, please check the playround site.
--
userinfo accessToken = doSimpleGetRequest (appendQueryParam
                                           "https://www.googleapis.com/oauth2/v2/userinfo"
	                                   (accessTokenToParam accessToken))
