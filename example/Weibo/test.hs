{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

{-

weibo oauth2: http://open.weibo.com/wiki/Oauth2

This is very trivial testing of the httpclient api.
1. this case will print out a URL
2. run the URL in browser and will navigate to weibo auth page
3. conform the authentication and browser will navigate back to the callback url,
   which obviously will failed cause there is no local server.
4. copy the `code` in the callback url and parse into console
5. this test case will gain access token using the `code` and print it out.

check for integration testing at:
https://github.com/HaskellCNOrg/snaplet-oauth/tree/master/test

-}

module Main where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import           URI.ByteString
import           URI.ByteString.QQ

import           Keys

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }


main :: IO ()
main = do
       print $ serializeURIRef' $ authorizationUrl weiboKey
       putStrLn "visit the url and paste code here: "
       code <- getLine
       mgr <- newManager tlsManagerSettings
       token <- fetchAccessToken mgr weiboKey (ExchangeToken $ T.pack code)
       print token
       case token of
         Right r -> do
                    uid <- authGetBS' mgr (accessToken r) [uri|https://api.weibo.com/2/account/get_uid.json|]
                    print (uid :: OAuth2Result (OAuth2Error Errors) BSL.ByteString)
         Left l -> print l

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
