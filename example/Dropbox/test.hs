{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad                 (liftM)
import           Data.Aeson.TH         (defaultOptions, deriveJSON)
import qualified Network.HTTP.Types            as HT
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Network.HTTP.Conduit

import           Network.OAuth.OAuth2

import           Keys


main :: IO ()
main = do
    print $ authorizationUrl dropboxKey
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr dropboxKey code
    print token
    case token of
      Right at  -> getSpaceUsage mgr at >>= print
      Left _    -> putStrLn "no access token found yet"

getSpaceUsage :: Manager -> AccessToken -> IO (OAuth2Result BSL.ByteString)
getSpaceUsage mgr token = do
  req <- parseRequest $ BS.unpack "https://api.dropboxapi.com/2/users/get_space_usage"
  authRequest req upReq mgr
  where upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
        upBody req = req {requestBody = "null" }
        upReq = upHeaders . upBody
