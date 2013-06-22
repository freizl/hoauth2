{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | A simple http client to request OAuth2 tokens and several utils.

module Network.OAuth.OAuth2.HttpClient where

import           Control.Monad                 (liftM)
import           Control.Monad.Trans.Resource  (ResourceT)
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
--import qualified Data.Text                    as T
--import qualified Data.Text.Encoding           as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types            (renderSimpleQuery)
import qualified Network.HTTP.Types            as HT

import           Network.OAuth.OAuth2.Internal

--------------------------------------------------
-- * Retrieve access token
--------------------------------------------------

-- | Request (via POST method) "Access Token".
--
--
fetchAccessToken :: OAuth2                           -- ^ OAuth Data
                   -> BS.ByteString                  -- ^ Authentication code gained after authorization
                   -> IO (OAuth2Result AccessToken)  -- ^ Access Token
fetchAccessToken oa code = doJSONPostRequest uri body
                           where (uri, body) = accessTokenUrl oa code


-- | Request the "Refresh Token".
fetchRefreshToken :: OAuth2                          -- ^ OAuth context
                     -> BS.ByteString                -- ^ refresh token gained after authorization
                     -> IO (OAuth2Result AccessToken)
fetchRefreshToken oa rtoken = doJSONPostRequest uri body
                              where (uri, body) = refreshAccessTokenUrl oa rtoken

--------------------------------------------------
-- * Simple POST requests
--------------------------------------------------

-- | Conduct post request and return response as JSON.
doJSONPostRequest :: FromJSON a
                  => URI                                 -- ^ The URL
                  -> PostBody                            -- ^ request body
                  -> IO (OAuth2Result a)                 -- ^ Response as ByteString
doJSONPostRequest uri body = liftM parseResponseJSON (doSimplePostRequest uri body)

-- | Conduct post request.
doSimplePostRequest :: URI                                  -- ^ URL
                       -> PostBody                          -- ^ Request body.
                       -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
doSimplePostRequest uri body =
    liftM handleResponse $ doPostRequestWithReq (BS.unpack uri) body id

-- | lower level api to do post request
doPostRequestWithReq :: String                               -- ^ URL
                    -> [(BS.ByteString, BS.ByteString)]      -- ^ Data to Post Body
                    -> (Request (ResourceT IO) -> Request (ResourceT IO))
                    -> IO (Response BSL.ByteString)          -- ^ Response
doPostRequestWithReq url body f = do
    req <- parseUrl url
    let req' = (f . updateRequestHeaders) req
    withManager $ httpLbs (urlEncodedBody body req')

--------------------------------------------------
-- * Simple GET requests
--------------------------------------------------

-- | Conduct GET request and return response as JSON.
doJSONGetRequest :: FromJSON a
                 => URI                          -- ^ Full URL
                 -> IO (OAuth2Result a)          -- ^ Response as JSON
doJSONGetRequest = liftM parseResponseJSON . doSimpleGetRequest

-- | Conduct GET request.
doSimpleGetRequest :: URI                               -- ^ URL
                   -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
doSimpleGetRequest url = liftM handleResponse (doGetRequestWithReq (BS.unpack url) [] id)


-- | lower level api to do get request
-- TODO: can not be `Request m -> Request m`, why??
--
doGetRequestWithReq :: String                                              -- ^ URL
                    -> [(BS.ByteString, BS.ByteString)]                    -- ^ Extra Parameters
                    -> (Request (ResourceT IO) -> Request (ResourceT IO))  -- ^ update Request
                    -> IO (Response BSL.ByteString)                        -- ^ Response
doGetRequestWithReq url pm f = do
    req <- parseUrl $ url ++ BS.unpack (renderSimpleQuery True pm)
    let req' = (f . updateRequestHeaders) req
    withManager $ httpLbs req'

--------------------------------------------------
-- * Utilities
--------------------------------------------------

handleResponse :: Response BSL.ByteString -> OAuth2Result BSL.ByteString
handleResponse rsp =
    if HT.statusCode (responseStatus rsp) == 200
        then Right $ responseBody rsp
        else Left $ BSL.append "Gaining token failed: " (responseBody rsp)

-- | Parses a @OAuth2Result BSL.ByteString@ into @FromJSON a => a@
parseResponseJSON :: FromJSON a
              => OAuth2Result BSL.ByteString
              -> OAuth2Result a
parseResponseJSON (Left b) = Left b
parseResponseJSON (Right b) = case decode b of
                            Nothing -> Left ("Could not decode JSON" `BSL.append` b)
                            Just x -> Right x

updateRequestHeaders :: Request m -> Request m
updateRequestHeaders req =
  let extras = [ (HT.hUserAgent, "hoauth2")
               , (HT.hAccept, "application/json") ]
      headers = extras ++ requestHeaders req
  in
  req { requestHeaders = headers }
