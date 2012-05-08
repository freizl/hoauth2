{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

{-
  A simple OAuth2 http client.
-}

module Network.OAuth2.HTTP.HttpClient where

import Control.Applicative ((<$>))
import Control.Exception
import Data.Aeson
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.HTTP.Types as HT
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Types (renderSimpleQuery)

import Network.OAuth2.OAuth2

--------------------------------------------------

-- | Request (POST method) access token URL in order to get @AccessToken@.
-- 
--   FIXME: what if @requestAccessToken'@ return error?
--
requestAccessToken :: OAuth2 
                -> BS.ByteString          -- ^ Authentication code gained after authorization
                -> IO (Maybe AccessToken)
requestAccessToken oa code = decode <$> requestAccessToken' oa code


requestAccessToken' :: OAuth2 -> BS.ByteString -> IO BSL.ByteString
requestAccessToken' oa code = doPostRequst (BS.unpack uri) body >>= retOrError
  where
    (uri, body) = accessTokenUrl oa code
    retOrError rsp = if (HT.statusCode . responseStatus) rsp == 200
                        --then (print $ responseBody rsp ) >> (return $ responseBody rsp)
                        then return $ responseBody rsp
                        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)


--------------------------------------------------
-- od Request Utils

-- TODO: Some duplication here.
-- TODO: Control.Exception.try
--        result <- liftIO $ Control.Exception.try $ runResourceT $ httpLbs request man
-- 
doSimpleGetRequest :: MonadIO m => String -> m (Response BSL.ByteString)
doSimpleGetRequest url = liftIO $ withManager $ \man -> do
    req' <- liftIO $ parseUrl url
    httpLbs req' man

doGetRequest :: MonadIO m => String -> [(BS.ByteString, BS.ByteString)] -> m (Response BSL.ByteString)
doGetRequest url pm = liftIO $ withManager $ \man -> do
    req' <- liftIO $ parseUrl $ url ++ BS.unpack (renderSimpleQuery True pm)
    httpLbs req' man

doPostRequst :: MonadIO m => String -> [(BS.ByteString, BS.ByteString)] -> m (Response BSL.ByteString)
doPostRequst url body = liftIO $ withManager $ \man -> do
    req' <- liftIO $ parseUrl url
    httpLbs (urlEncodedBody body req') man

