{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-

This is basically very manual test. Check following link for details.

Google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Generics
import           Network.HTTP.Conduit
import           Prelude                       hiding (id)
import           System.Environment            (getArgs)
import           URI.ByteString
import           URI.ByteString.QQ

import           Keys                          (googleKey)
import           Network.OAuth.OAuth2

--------------------------------------------------

data Token = Token { issuedTo   :: Text
                   , audience   :: Text
                   , userId     :: Maybe Text
                   , scope      :: Text
                   , expiresIn  :: Integer
                   , accessType :: Text
                   } deriving (Show, Generic)


data User = User { id         :: Text
                 , name       :: Text
                 , givenName  :: Text
                 , familyName :: Text
                 , link       :: Text
                 , picture    :: Text
                 , gender     :: Text
                 , locale     :: Text
                 } deriving (Show, Generic)

instance FromJSON Token where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Token where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = camelTo2 '_' }

--------------------------------------------------

main :: IO ()
main = do
    xs <- getArgs
    mgr <- newManager tlsManagerSettings
    case xs of
        ["offline"] -> offlineCase mgr
        _           -> normalCase mgr

offlineCase :: Manager -> IO ()
offlineCase mgr = do
    BS.putStrLn $ serializeURIRef' $ appendQueryParams (googleScopeEmail ++ googleAccessOffline) $ authorizationUrl googleKey
    putStrLn "visit the url and paste code here: "
    code <- getLine
    fetchAccessToken mgr googleKey $ ExchangeToken $ T.pack code
    (Right token) <- fetchAccessToken mgr googleKey $ ExchangeToken $ T.pack code
    f (accessToken token)
    --
    -- obtain a new access token with refresh token, which turns out only in response at first time.
    -- Revoke Access https://www.google.com/settings/security
    --
    case refreshToken token of
        Nothing -> putStrLn "Failed to fetch refresh token"
        Just tk -> do
            (Right token') <- fetchRefreshToken mgr googleKey tk
            f token'
            --validateToken accessToken >>= print
            --(validateToken' accessToken :: IO (OAuth2Result Token)) >>= print
    where f token = do
            print token
            validateToken mgr token >>= print
            (validateToken' mgr token :: IO (OAuth2Result Token)) >>= print

normalCase :: Manager -> IO ()
normalCase mgr = do
    -- try an invalid token
    putStr "Trying invalid token..."
    validateToken mgr (AccessToken "invalid") >>= print
    BS.putStrLn $ serializeURIRef' $ appendQueryParams googleScopeUserInfo (authorizationUrl googleKey)
    putStrLn "visit the url and paste code here: "
    code <- fmap (ExchangeToken . T.pack) getLine
    maybeToken <- fetchAccessToken mgr googleKey code
    print maybeToken
    (Right token) <- return maybeToken
    putStr "AccessToken: " >> print token
    -- get response in ByteString
    validateToken mgr (accessToken token) >>= print
    -- get response in JSON
    (validateToken' mgr (accessToken token):: IO (OAuth2Result Token)) >>= print
    -- get response in ByteString
    userinfo mgr (accessToken token) >>= print
    -- get response in JSON
    (userinfo' mgr (accessToken token) :: IO (OAuth2Result User)) >>= print

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
validateToken :: Manager
                 -> AccessToken
                 -> IO (OAuth2Result BL.ByteString)
validateToken mgr token =
   authGetBS' mgr token url
   where url = [uri|https://www.googleapis.com/oauth2/v1/tokeninfo|]

validateToken' :: FromJSON a
                  => Manager
                  -> AccessToken
                  -> IO (OAuth2Result a)
validateToken' mgr token = parseResponseJSON <$> validateToken mgr token

-- | fetch user email.
--   for more information, please check the playround site.
--
userinfo :: Manager
            -> AccessToken
            -> IO (OAuth2Result BL.ByteString)
userinfo mgr token = authGetBS mgr token [uri|https://www.googleapis.com/oauth2/v2/userinfo|]

userinfo' :: FromJSON a
             => Manager
             -> AccessToken
             -> IO (OAuth2Result a)
userinfo' mgr token = authGetJSON mgr token [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
