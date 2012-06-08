{-# LANGUAGE OverloadedStrings #-}

{-

This is basically very manual test. Check following link for details.

google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import System

import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
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
          print $ authorizationUrl gauth `BS.append` "&" `BS.append` extraParams
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken accessToken refreshToken)) <- requestAccessToken gauth (BS.pack code) 
          print (accessToken, refreshToken)
          validateToken accessToken >>= print
          -- 
          -- obtain a new access token with refresh token, which turns out only in response at first time.
          -- Revoke Access https://www.google.com/settings/security
          -- 
          refreshAccessToken gauth (fromJust refreshToken) >>= print
    where extraParams = renderSimpleQuery False $ ("access_type", "offline"):googleScopeEmail


normalCase :: IO ()
normalCase = do 
          print $ authorizationUrl gauth `BS.append` "&" `BS.append` extraParams
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken accessToken Nothing)) <- requestAccessToken gauth (BS.pack code) 
          print accessToken
          res <- validateToken accessToken
          print res
    where extraParams = renderSimpleQuery False googleScopeEmail

--------------------------------------------------

gauth :: OAuth2
gauth = googleKeys { oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token" 
                   , oauthAccessToken = Nothing
                   }

--------------------------------------------------
-- Google API

-- | this is special for google Gain read-only access to the user's email address.
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

-- | Gain read-only access to basic profile information, including a 
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- Token Validation
validateToken accessToken = postRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo", 
                                         (accessTokenToParam accessToken))