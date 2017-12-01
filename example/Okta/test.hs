{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T
import           Data.Aeson
import           Data.Aeson.Types
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types         as HT
import           URI.ByteString
import           GHC.Generics
import           URI.ByteString.QQ

import           Network.OAuth.OAuth2

import           Keys

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

main :: IO ()
main = do
    BS.putStrLn
      $ serializeURIRef'
      $ appendQueryParams [("scope", "openid profile"), ("state", "test-state-123")]
      $ authorizationUrl oktaKey
    putStrLn "visit the url and paste code here: "
    code <- getLine
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr oktaKey (ExchangeToken (T.pack code))
    print token
    case token of
      Right at -> getUserInfo mgr (accessToken at) >>= print
      Left _   -> putStrLn "no access token found yet"

getUserInfo :: Manager -> AccessToken -> IO (OAuth2Result Errors BSL.ByteString)
getUserInfo mgr token =
  authGetBS mgr token [uri|https://dev-148986.oktapreview.com/oauth2/v1/userinfo|]
