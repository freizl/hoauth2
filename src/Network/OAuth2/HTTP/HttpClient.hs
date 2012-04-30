{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  A simple OAuth2 http client.
-}

module Network.OAuth2.HTTP.HttpClient 
       ( requestAccessToken
       , doRequest
       , signRequest
       ) where

import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List
import Data.Maybe
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit
import Control.Exception
import Control.Applicative ((<$>))

import Network.OAuth2.OAuth2

--------------------------------------------------

-- | Request (POST method) access token URL in order to get @AccessToken@.
--   FIXME: what if @requestAccessToken'@ return error?
requestAccessToken :: OAuth2 
                -> BS.ByteString          -- ^ Authentication code gained after authorization
                -> IO (Maybe AccessToken)
requestAccessToken oa code = decode <$> requestAccessToken' oa code


requestAccessToken' :: OAuth2 -> BS.ByteString -> IO BSL.ByteString
requestAccessToken' oa code = doRequest req >>= retOrError
  where
    req = urlEncodedBody body $ toReq' uri
    (uri, body) = accessTokenUrl oa code
    retOrError rsp = if (HT.statusCode . statusCode) rsp == 200
                        then return $ responseBody rsp
                        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)


-- | insert access token into the request
signRequest :: OAuth2 -> Request m -> Request m
signRequest oa req = req { queryString = renderSimpleQuery False newQuery }
  where
    newQuery = case oauthAccessToken oa of
                    Just at -> insert ("access_token", at) oldQuery
                    _ -> insert ("client_id", oauthClientId oa) . insert ("client_secret", oauthClientSecret oa) $ oldQuery
    oldQuery = parseSimpleQuery (queryString req)

--------------------------------------------------
-- UTIL

toReq' :: BS.ByteString -> Request a
toReq' = fromJust . parseUrl . BS.unpack

-- | Performance a http @Request@
doRequest :: ResourceIO m => Request m -> m (Response BSL.ByteString)
doRequest = withManager . httpLbs
