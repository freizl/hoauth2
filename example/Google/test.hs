{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

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
import Keys

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Conduit (Response)
import System.Environment
import Prelude hiding (id)
import qualified Prelude as P (id)

--------------------------------------------------

data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   , access_type :: Text
                   } deriving (Show)

$(deriveJSON P.id ''Token)

data User = User { id         :: Text
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
    code <- getLine
    (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKey (BS.pack code)
    print (accessToken, refreshToken)
    validateToken accessToken >>= print
    --
    -- obtain a new access token with refresh token, which turns out only in response at first time.
    -- Revoke Access https://www.google.com/settings/security
    --
    case refreshToken of
        Nothing -> print "Failed to fetch refresh token"
        Just tk -> refreshAccessToken googleKey tk >>= print

normalCase :: IO ()
normalCase = do
    print $ authorizationUrl googleKey `appendQueryParam'` googleScopeUserInfo
    putStrLn "visit the url and paste code here: "
    code <- getLine
    (Just (AccessToken accessToken Nothing)) <- requestAccessToken googleKey (BS.pack code)
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
googleAccessOffline = [("access_type", "offline")]

-- Token Validation
validateToken :: BS.ByteString -> IO BL.ByteString
validateToken accessToken = doSimplePostRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo", (accessTokenToParam accessToken))

validateToken' :: FromJSON a => BS.ByteString -> IO (Maybe a)
validateToken' accessToken = doJSONGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

-- | fetch user email.
--   for more information, please check the playround site.
--
userinfo :: BS.ByteString -> IO BL.ByteString
userinfo accessToken = doSimpleGetRequest (appendQueryParam "https://www.googleapis.com/oauth2/v2/userinfo" (accessTokenToParam accessToken))

userinfo' :: FromJSON a => BS.ByteString -> IO (Maybe a)
userinfo' accessToken = doJSONGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo" `appendQueryParam` (accessTokenToParam accessToken))

