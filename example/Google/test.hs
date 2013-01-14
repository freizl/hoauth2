{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

{-

This is basically very manual test. Check following link for details.

Google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Main where

import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2
import Keys (googleKey)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:), (.:?))
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Text (Text)
import Prelude hiding (id)
import qualified Prelude as P (id)
import System.Environment (getArgs)

--------------------------------------------------

data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Maybe Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   , email       :: Maybe Text
                   , verified_email :: Maybe Bool
                   , access_type :: Text
                   } deriving (Show)

instance FromJSON Token where
    parseJSON (Object o) = Token
                           <$> o .:  "issued_to"
                           <*> o .:  "audience"
                           <*> o .:? "user_id"
                           <*> o .:  "scope"
                           <*> o .:  "expires_in"
                           <*> o .:? "email"
                           <*> o .:? "verified_email"
                           <*> o .:  "access_type"
    parseJSON _ = mzero

data User = User { id          :: Text
                 , name        :: Text
                 , given_name  :: Text
                 , family_name :: Text
                 , link        :: Text
                 , picture     :: Text
                 , gender      :: Text
                 , birthday    :: Text
                 , locale      :: Text
                 } deriving (Show)

$(deriveJSON P.id ''User)

--------------------------------------------------

main :: IO ()
main = do
    xs <- getArgs
    case xs of
        ["offline"] -> offlineCase
        _ -> normalCase

offlineCase :: IO ()
offlineCase = do
    print $ authorizationUrl googleKey `appendQueryParam'` googleScopeEmail `appendQueryParam'` googleAccessOffline
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKey code
    print (accessToken, refreshToken)
    validateToken accessToken >>= print
    (validateToken' accessToken :: IO (Maybe Token)) >>= print
    --
    -- obtain a new access token with refresh token, which turns out only in response at first time.
    -- Revoke Access https://www.google.com/settings/security
    --
    case refreshToken of
        Nothing -> print "Failed to fetch refresh token"
        Just tk -> do
            (Just (AccessToken accessToken refreshToken)) <- refreshAccessToken googleKey tk
            print (accessToken, refreshToken)
            validateToken accessToken >>= print
            (validateToken' accessToken :: IO (Maybe Token)) >>= print

normalCase :: IO ()
normalCase = do
    print $ authorizationUrl googleKey `appendQueryParam'` googleScopeUserInfo
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Just (AccessToken accessToken Nothing)) <- requestAccessToken googleKey code
    putStr "AccessToken: " >> print accessToken
    validateToken accessToken >>= print
    (validateToken' accessToken :: IO (Maybe Token)) >>= print
    userinfo accessToken >>= print
    (userinfo' accessToken :: IO (Maybe User)) >>= print

--------------------------------------------------
-- Google API

-- | This is special for google Gain read-only access to the user's email address.
googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

-- | Gain read-only access to basic profile information, including a
googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

-- | Access offline
googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

-- | Token Validation
validateToken :: BS.ByteString -> IO BL.ByteString
validateToken accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

validateToken' :: FromJSON a => BS.ByteString -> IO (Maybe a)
validateToken' accessToken = doJSONGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

-- | fetch user email.
--   for more information, please check the playround site.
--
userinfo :: BS.ByteString -> IO BL.ByteString
userinfo accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo" `appendQueryParam` (accessTokenToParam accessToken))

userinfo' :: FromJSON a => BS.ByteString -> IO (Maybe a)
userinfo' accessToken = doJSONGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo" `appendQueryParam` (accessTokenToParam accessToken))

