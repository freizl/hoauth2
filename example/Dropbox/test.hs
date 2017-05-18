{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

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
      Right at -> getSpaceUsage mgr (accessToken at) >>= print
      Left _   -> putStrLn "no access token found yet"

getSpaceUsage :: Manager -> AccessToken -> IO (OAuth2Result Errors BSL.ByteString)
getSpaceUsage mgr token = do
  req <- parseRequest $ BS.unpack "https://api.dropboxapi.com/2/users/get_space_usage"
  authRequest req upReq mgr
  where upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
        upBody req = req {requestBody = "null" }
        upReq = upHeaders . upBody
