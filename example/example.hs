{-# LANGUAGE OverloadedStrings #-}
module Example where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Network.HTTP.Conduit (Response)
import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
import System.Environment

googleKeys :: OAuth2
googleKeys = OAuth2 { oauthClientId = "xxxxxxxxxxxx.apps.googleusercontent.com"
                    , oauthClientSecret = "xxxxxxxxxxxxxxxxx"
                    , oauthCallback = Just "http://localhost:9160/state.htm"
                    , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                    , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                    , oauthAccessToken = Nothing}

validateToken :: BS.ByteString -> IO (Response BL.ByteString)
validateToken accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

userinfo :: BS.ByteString -> IO (Response BL.ByteString)
userinfo accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo"  `appendQueryParam` (accessTokenToParam accessToken))

main :: IO ()
main = do
    (x:_) <- getArgs
    case x of
        "normal" -> normalCase
        "offline" -> offlineCase

normalCase :: IO ()
normalCase = do
    print $ (authorizationUrl googleKeys) `appendQueryParam'` [("scope", "https://www.googleapis.com/auth/userinfo.profile")]
    putStrLn "visit the url and paste code here: "
    code <- getLine
    (Just (AccessToken accessToken Nothing)) <- requestAccessToken googleKeys (BS.pack code)
    validateToken accessToken >>= print
    userinfo accessToken >>= print

offlineCase :: IO ()
offlineCase = do
    print $ (authorizationUrl googleKeys) `appendQueryParam'` [("scope", "https://www.googleapis.com/auth/userinfo.profile"), ("access_type", "offline")]
    putStrLn "visit the url and paste code here: "
    code <- getLine
    (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKeys (BS.pack code)
    case refreshToken of
        Nothing -> print "Failed to fetch refresh token"
        Just tk -> do
            (Just (AccessToken accessToken Nothing)) <- refreshAccessToken googleKeys tk
            validateToken accessToken >>= print
            userinfo accessToken >>= print

