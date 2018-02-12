{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T
import           GHC.Generics
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types         as HT
import           URI.ByteString

import           Network.OAuth.OAuth2

import           Keys

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

main :: IO ()
main = do
    BS.putStrLn $ serializeURIRef' $ authorizationUrl dropboxKey
    putStrLn "visit the url and paste code here: "
    code <- getLine
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr dropboxKey (ExchangeToken (T.pack code))
    print token
    case token of
      Right at -> getAccount mgr (accessToken at) >>= print
      Left _   -> putStrLn "no access token found yet"

getAccount :: Manager -> AccessToken -> IO (OAuth2Result Errors BSL.ByteString)
getAccount mgr token = do
  req <- parseRequest $ BS.unpack "https://api.dropboxapi.com/2/users/get_current_account"
  authRequest req upReq mgr
  where upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
        upBody req = req {requestBody = "null" }
        upReq = upHeaders . upBody
