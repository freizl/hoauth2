{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Conduit                    (MonadResource)
import           Data.Maybe                      (fromJust)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types              as HT

import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import           Keys

main :: IO ()
main = do
       print $ authorizationUrl weiboKey
       putStrLn "visit the url and paste code here: "
       code <- getLine
       mgr <- newManager conduitManagerSettings
       token <- fetchAccessToken mgr weiboKey (sToBS code)
       print token
       case token of
         Right r -> do
                    uid <- authGetBS mgr r "https://api.weibo.com/2/account/get_uid.json"
                    print uid
         Left l -> BSL.putStrLn l
       closeManager mgr

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
