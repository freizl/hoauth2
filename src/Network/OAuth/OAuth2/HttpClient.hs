{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | A simple http client to request OAuth2 tokens and several utils.

module Network.OAuth.OAuth2.HttpClient where

import           Control.Monad                 (liftM)
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Maybe
import           Network.HTTP.Conduit          hiding (withManager)
import qualified Network.HTTP.Types            as HT

import           Network.OAuth.OAuth2.Internal

--------------------------------------------------
-- * Retrieve access token
--------------------------------------------------

-- | Request (via POST method) "Access Token".
--
--
fetchAccessToken :: OAuth2                           -- ^ OAuth Data
                   -> Manager                        -- ^ HTTP connection manager.
                   -> BS.ByteString                  -- ^ Authentication code gained after authorization
                   -> IO (OAuth2Result AccessToken)  -- ^ Access Token
fetchAccessToken oa manager code = doJSONPostRequest oa manager uri body
                           where (uri, body) = accessTokenUrl oa code


-- | Request the "Refresh Token".
fetchRefreshToken :: OAuth2                          -- ^ OAuth context
                     -> Manager                      -- ^ HTTP connection manager.
                     -> BS.ByteString                -- ^ refresh token gained after authorization
                     -> IO (OAuth2Result AccessToken)
fetchRefreshToken oa manager rtoken = doJSONPostRequest oa manager uri body
                              where (uri, body) = refreshAccessTokenUrl oa rtoken


-- | Conduct post request and return response as JSON.
doJSONPostRequest :: FromJSON a
                  => OAuth2
                  -> Manager                             -- ^ HTTP connection manager.
                  -> URI                                 -- ^ The URL
                  -> PostBody                            -- ^ request body
                  -> IO (OAuth2Result a)                 -- ^ Response as ByteString
doJSONPostRequest oa manager uri body = liftM parseResponseJSON (doSimplePostRequest oa manager uri body)

-- | Conduct post request.
doSimplePostRequest :: OAuth2
                       -> Manager                           -- ^ HTTP connection manager.
                       -> URI                               -- ^ URL
                       -> PostBody                          -- ^ Request body.
                       -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
doSimplePostRequest oa manager url body = liftM handleResponse go
                                  where go = do
                                             req <- parseUrl $ BS.unpack url
                                             let addBasicAuth = applyBasicAuth (oauthClientId oa) (oauthClientSecret oa)
                                                 req' = (addBasicAuth . updateRequestHeaders Nothing) req
                                             httpLbs (urlEncodedBody body req') manager

--------------------------------------------------
-- * AUTH requests
--------------------------------------------------

-- | Conduct GET request and return response as JSON.
authGetJSON :: FromJSON a
                 => Manager                      -- ^ HTTP connection manager.
                 -> AccessToken
                 -> URI                          -- ^ Full URL
                 -> IO (OAuth2Result a)          -- ^ Response as JSON
authGetJSON manager t uri = liftM parseResponseJSON $ authGetBS manager t uri

-- | Conduct GET request.
authGetBS :: Manager                              -- ^ HTTP connection manager.
             -> AccessToken
             -> URI                               -- ^ URL
             -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
authGetBS manager token url = liftM handleResponse go
                      where go = do
                                 req <- parseUrl $ BS.unpack $ url `appendAccessToken` token
                                 authenticatedRequest manager token HT.GET req

-- | Conduct POST request and return response as JSON.
authPostJSON :: FromJSON a
                 => Manager                      -- ^ HTTP connection manager.
                 -> AccessToken
                 -> URI                          -- ^ Full URL
                 -> PostBody
                 -> IO (OAuth2Result a)          -- ^ Response as JSON
authPostJSON manager t uri pb = liftM parseResponseJSON $ authPostBS manager t uri pb

-- | Conduct POST request.
authPostBS :: Manager                             -- ^ HTTP connection manager.
             -> AccessToken
             -> URI                               -- ^ URL
             -> PostBody
             -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
authPostBS manager token url pb = liftM handleResponse go
                          where body = pb ++ accessTokenToParam token
                                go = do
                                     req <- parseUrl $ BS.unpack url
                                     authenticatedRequest manager token HT.POST $  urlEncodedBody body req


-- |Sends a HTTP request including the Authorization header with the specified
--  access token.
--
authenticatedRequest :: Manager                          -- ^ HTTP connection manager.
                     -> AccessToken                      -- ^ Authentication token to use
                     -> HT.StdMethod                     -- ^ Method to use
                     -> Request        -- ^ Request to perform
                     -> IO (Response BSL.ByteString)
authenticatedRequest manager token m r =
    httpLbs (updateRequestHeaders (Just token) $ setMethod m r) manager

-- | Sets the HTTP method to use
--
setMethod :: HT.StdMethod -> Request -> Request
setMethod m req = req { method = HT.renderStdMethod m }

--------------------------------------------------
-- * Utilities
--------------------------------------------------

-- | Parses a @Response@ to to @OAuth2Result@
--
handleResponse :: Response BSL.ByteString -> OAuth2Result BSL.ByteString
handleResponse rsp =
    if HT.statusCode (responseStatus rsp) == 200
        then Right $ responseBody rsp
        else Left $ BSL.append "Gaining token failed: " (responseBody rsp)

-- | Parses a @OAuth2Result BSL.ByteString@ into @FromJSON a => a@
--
parseResponseJSON :: FromJSON a
              => OAuth2Result BSL.ByteString
              -> OAuth2Result a
parseResponseJSON (Left b) = Left b
parseResponseJSON (Right b) = case decode b of
                            Nothing -> Left ("Could not decode JSON" `BSL.append` b)
                            Just x -> Right x

-- | set several header values.
--   + userAgennt : hoauth2
--   + accept     : application/json
--   + authorization : Bearer xxxxx  if AccessToken provided.
--
updateRequestHeaders :: Maybe AccessToken -> Request -> Request
updateRequestHeaders t req =
  let extras = [ (HT.hUserAgent, "hoauth2")
               , (HT.hAccept, "application/json") ]
      bearer = [(HT.hAuthorization, "Bearer " `BS.append` accessToken (fromJust t)) | isJust t]
      headers = bearer ++ extras ++ requestHeaders req
  in
  req { requestHeaders = headers }
