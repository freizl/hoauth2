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

TODO:
  1. a simple local server in order to make the test automatically.
-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit

import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2

import WeiboKey

weibooauth :: OAuth2
weibooauth = weiboKey { oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                      , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token" 
                      , oauthAccessToken = Nothing
                      }

urlToRequest = fromJust . parseUrl

accountUidReq = urlToRequest "https://api.weibo.com/2/account/get_uid.json"

-- | send a request and get a response body if successfully otherwise error
sendRequest :: Request IO -> IO BSL.ByteString
sendRequest req = do
    rsp <- doRequest req
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else return $ BSL.pack $ "Error when requesting: " ++ BSL.unpack (responseBody rsp)
  
-- | fetch UID    
main :: IO ()
main = do 
          print $ authorizationUrl weibooauth
          putStr "visit the url and paste code here: "
          code <- getLine
          token <- requestAccessToken weibooauth (BS.pack code)
          case token of 
            Just (AccessToken token') -> do
                                         req <- return $ signRequest (weibooauth { oauthAccessToken = Just token'} ) accountUidReq
                                         print (queryString req)
                                         sendRequest req >>= print
            _ -> print "no token found"
