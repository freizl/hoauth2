{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | A simple http client to request OAuth2 tokens and several utils.

module Network.OAuth.OAuth2.HttpClient where

import           Control.Monad                (liftM)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as BSL
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types           (renderSimpleQuery)
import qualified Network.HTTP.Types           as HT

import           Network.OAuth.OAuth2.Internal

--------------------------------------------------
-- * Retrieve access token
--------------------------------------------------

-- | Request (via POST method) "Access Token".
--
--   FIXME: what if @requestAccessToken@ return error?
--
requestAccessToken :: OAuth2              -- ^ OAuth Data
                   -> BS.ByteString       -- ^ Authentication code gained after authorization
                   -> IO (OAuth2Result AccessToken) -- ^ Access Token
requestAccessToken oa code = doJSONPostRequest (accessTokenUrl oa code)


-- | Request the "Refresh Token".
refreshAccessToken :: OAuth2          -- ^ OAuth context
                   -> BS.ByteString   -- ^ refresh token gained after authorization
                   -> IO (OAuth2Result AccessToken)
refreshAccessToken oa rtoken = doJSONPostRequest (refreshAccessTokenUrl oa rtoken)

--------------------------------------------------
-- * Utilities
--------------------------------------------------

handleResponse :: Response BSL.ByteString -> OAuth2Result BSL.ByteString
handleResponse rsp =
    if (HT.statusCode $ responseStatus rsp) == 200
        then Right $ responseBody rsp
        else Left $ BSL.append "Gaining token failed: " (responseBody rsp)

-- |Parses a @OAuth2Result BSL.ByteString@ into @FromJSON a => a@
parseResponse :: FromJSON a
              => OAuth2Result BSL.ByteString
              -> OAuth2Result a
parseResponse rsp =
    either (Left) -- Return Left if error
           (maybe (Left "Could not decode JSON") (Right) . decode) -- Decode JSON
           rsp

updateRequestHeaders :: Request m -> Request m
updateRequestHeaders req = req { requestHeaders = [ (HT.hUserAgent, "hoauth2"), (HT.hAccept, "application/json") ] }

--------------------------------------------------
-- * Simple HTTP requests
--------------------------------------------------

-- | Conduct post request.
doSimplePostRequest :: (URI, PostBody)                   -- ^ The URI and request body for fetching token.
                    -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
doSimplePostRequest (uri, body) =
    fmap handleResponse $ doPostRequest (BS.unpack uri) body

-- | Conduct post request and return response as JSON.
doJSONPostRequest :: FromJSON a
                  => (URI, PostBody)  -- ^ The URI and request body for fetching token.
                  -> IO (OAuth2Result a)     -- ^ Response as ByteString
doJSONPostRequest (uri, body) = do
    rsp <- fmap handleResponse $ doPostRequest (BS.unpack uri) body
    case rsp of
      Left err -> return $ Left err
      Right a -> case decode a of
                    Nothing -> return $ Left "JSON: Decoding error"
                    Just r  -> return $ Right r

-- | Conduct POST request with given URL with post body data.
doPostRequest ::  String                               -- ^ URL
              -> [(BS.ByteString, BS.ByteString)]  -- ^ Data to Post Body
              -> IO (Response BSL.ByteString)      -- ^ Response
doPostRequest url body = doPostRequestWithReq url body id


doPostRequestWithReq ::  String                               -- ^ URL
                    -> [(BS.ByteString, BS.ByteString)]  -- ^ Data to Post Body
                    -> (Request (ResourceT IO)
                    -> Request (ResourceT IO))
                    -> IO (Response BSL.ByteString)      -- ^ Response
doPostRequestWithReq url body f = do
    req <- parseUrl url
    let req' = (updateRequestHeaders . f) req
    withManager $ httpLbs (urlEncodedBody body req')

-- | Conduct GET request with given URL by append extra parameters provided.
doGetRequest :: String                               -- ^ URL
             -> [(BS.ByteString, BS.ByteString)]  -- ^ Extra Parameters
             -> IO (Response BSL.ByteString)      -- ^ Response
doGetRequest url pm = doGetRequestWithReq url pm id

-- | TODO: can not be `Request m -> Request m`, why??
doGetRequestWithReq :: String                               -- ^ URL
                    -> [(BS.ByteString, BS.ByteString)]  -- ^ Extra Parameters
                    -> (Request (ResourceT IO) -> Request (ResourceT IO))          -- ^ update Request
                    -> IO (Response BSL.ByteString)      -- ^ Response
doGetRequestWithReq url pm f = do
    req <- parseUrl $ url ++ BS.unpack (renderSimpleQuery True pm)
    let req' = (updateRequestHeaders .f) req
    withManager $ httpLbs req'

-- | Conduct GET request and return response as JSON.
doJSONGetRequest :: FromJSON a
                 => URI         -- ^ Full URL
                 -> IO (OAuth2Result a) -- ^ Response as JSON
doJSONGetRequest url =
    fmap (parseResponse . handleResponse) $ doGetRequest (BS.unpack url) []

-- | Conduct GET request.
doSimpleGetRequest :: URI                               -- ^ URL
                   -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
doSimpleGetRequest url = doGetRequest (BS.unpack url) [] >>= return . handleResponse

