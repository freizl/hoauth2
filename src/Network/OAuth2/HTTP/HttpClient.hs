{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | A simple http client for request OAuth2 tokens and several utils.

module Network.OAuth2.HTTP.HttpClient where

import           Control.Applicative        ((<$>))
import           Control.Exception
import           Control.Monad.Trans        (liftIO)
import Control.Monad (liftM)
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types         (renderSimpleQuery)
import qualified Network.HTTP.Types         as HT

import           Network.OAuth2.OAuth2

--------------------------------------------------

-- | Request (via POST method) "Access Token".
--
--   FIXME: what if @requestAccessToken'@ return error?
--
requestAccessToken :: OAuth2                 -- ^ OAuth Data
                   -> BS.ByteString          -- ^ Authentication code gained after authorization
                   -> IO (Maybe AccessToken) -- ^ Access Token
requestAccessToken oa code = decode <$> doSimplePostRequest (accessTokenUrl oa code)


-- | Request the "Refresh Token".
--
refreshAccessToken :: OAuth2
                   -> BS.ByteString          -- ^ refresh token gained after authorization
                   -> IO (Maybe AccessToken)
refreshAccessToken oa rtoken = decode <$> doSimplePostRequest (refreshAccessTokenUrl oa rtoken)


--------------------------------------------------

-- | Conduct post request.
--
doSimplePostRequest :: (URI, PostBody)       -- ^ The URI and request body for fetching token.
                       -> IO BSL.ByteString  -- ^ Response as ByteString
doSimplePostRequest (uri, body) = doPostRequst (bsToS uri) body >>= handleResponse

-- | Conduct post request and return response as JSON.
-- 
doJSONPostRequest :: FromJSON a
                     => (URI, PostBody)  -- ^ The URI and request body for fetching token.
                     -> IO (Maybe a)     -- ^ Response as ByteString
doJSONPostRequest (uri, body) = doPostRequst (bsToS uri) body
                                >>= liftM decode . handleResponse


-- | Conduct GET request.
--
doSimpleGetRequest :: String                           -- ^ URL
                      -> IO (Response BSL.ByteString)  -- ^ Response as ByteString
doSimpleGetRequest url = liftIO $ withManager $ \man -> do
    req' <- liftIO $ parseUrl url
    let req'' = req' { requestHeaders = [("Content-Type", "application/json")] }
    httpLbs req'' man

-- | Conduct GET request and return response as JSON.
--
doJSONGetRequest :: FromJSON a
                      => String         -- ^ Full URL
                      -> IO (Maybe a)   -- ^ Response as ByteString
doJSONGetRequest url = doGetRequest url []
                         >>= liftM decode . handleResponse

  --liftIO $ withManager $ \man -> do
  --  req' <- liftIO $ parseUrl url
  --  httpLbs req' man


--------------------------------------------------
-- conduit http request
--------------------------------------------------

-- | Conduct GET request with given URL by append extra parameters provided.
--
doGetRequest :: String                               -- ^ URL
                -> [(BS.ByteString, BS.ByteString)]  -- ^ Extra Parameters
                -> IO (Response BSL.ByteString)      -- ^ Response
doGetRequest url pm = liftIO $ withManager $ \man -> do
    req' <- parseUrl $ url ++ bsToS (renderSimpleQuery True pm)
    let req'' = req' { requestHeaders = [("Content-Type", "application/json")] }
    httpLbs req'' man


-- | Conduct POST request with given URL with post body data.
--
doPostRequst ::  String                              -- ^ URL
                -> [(BS.ByteString, BS.ByteString)]  -- ^ Data to Post Body
                -> IO (Response BSL.ByteString)      -- ^ Response
doPostRequst url body = liftIO $ withManager $ \man -> do
    req' <- parseUrl url
    let req'' = req' { requestHeaders = [("Content-Type", "application/json")] }
    httpLbs (urlEncodedBody body req'') man


--------------------------------------------------

handleResponse :: Response BSL.ByteString -> IO BSL.ByteString
handleResponse rsp =  if (HT.statusCode . responseStatus) rsp == 200
                     then return (responseBody rsp)
                     else throwIO . OAuthException $
                          "Gaining token failed: " ++ BSL.unpack (responseBody rsp)


bsToS ::  BS.ByteString -> String
bsToS = T.unpack . T.decodeUtf8

                      --else BSL.putStrLn  "Error rsp" >> return "ERROR"

-- TODO: Control.Exception.try
--        result <- liftIO $ Control.Exception.try $ runResourceT $ httpLbs request man
