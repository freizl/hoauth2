{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple http client to request OAuth2 tokens and several utils.

module Network.OAuth.OAuth2.HttpClient (
-- * Token management
  fetchAccessToken,
  fetchRefreshToken,
  doJSONPostRequest,
  doFlexiblePostRequest,
  doSimplePostRequest,
-- * AUTH requests
  authGetJSON,
  authGetBS,
  authGetBS',
  authPostJSON,
  authPostBS,
  authPostBS',
  authRequest,
-- * Utilities
  handleResponse,
  parseResponseJSON,
  parseResponseFlexible,
  updateRequestHeaders,
  setMethod
) where

import           Control.Monad                 (liftM)
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.HashMap.Strict           as HM (fromList)
import           Data.Maybe
import qualified Data.Text.Encoding            as T
import           Network.HTTP.Conduit          hiding (withManager)
import qualified Network.HTTP.Types            as HT
import           Network.HTTP.Types.URI        (parseQuery)
import           Network.OAuth.OAuth2.Internal
import           URI.ByteString

--------------------------------------------------
-- * Token management
--------------------------------------------------

-- | Request (via POST method) "OAuth2 Token".
fetchAccessToken :: Manager                          -- ^ HTTP connection manager
                   -> OAuth2                         -- ^ OAuth Data
                   -> ExchangeToken                  -- ^ OAuth 2 Tokens
                   -> IO (OAuth2Result OAuth2Token)  -- ^ Access Token
fetchAccessToken manager oa code = doFlexiblePostRequest manager oa uri body
                           where (uri, body) = accessTokenUrl oa code


-- | Request the "Refresh Token".
fetchRefreshToken :: Manager                         -- ^ HTTP connection manager.
                     -> OAuth2                       -- ^ OAuth context
                     -> RefreshToken                 -- ^ refresh token gained after authorization
                     -> IO (OAuth2Result AccessToken)
fetchRefreshToken manager oa token = doFlexiblePostRequest manager oa uri body
                              where (uri, body) = refreshAccessTokenUrl oa token


-- | Conduct post request and return response as JSON.
doJSONPostRequest :: FromJSON a
                  => Manager                             -- ^ HTTP connection manager.
                  -> OAuth2                              -- ^ OAuth options
                  -> URI                     -- ^ The URL
                  -> PostBody                            -- ^ request body
                  -> IO (OAuth2Result a)                 -- ^ Response as ByteString
doJSONPostRequest manager oa uri body = liftM parseResponseJSON (doSimplePostRequest manager oa uri body)

-- | Conduct post request and return response as JSON or Query String.
doFlexiblePostRequest :: FromJSON a
                         => Manager                             -- ^ HTTP connection manager.
                         -> OAuth2                              -- ^ OAuth options
                         -> URI                     -- ^ The URL
                         -> PostBody                            -- ^ request body
                         -> IO (OAuth2Result a)                 -- ^ Response as ByteString
doFlexiblePostRequest manager oa uri body = liftM parseResponseFlexible (doSimplePostRequest manager oa uri body)

-- | Conduct post request.
doSimplePostRequest :: Manager                              -- ^ HTTP connection manager.
                       -> OAuth2                            -- ^ OAuth options
                       -> URI                   -- ^ URL
                       -> PostBody                          -- ^ Request body.
                       -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
doSimplePostRequest manager oa url body = liftM handleResponse go
                                  where go = do
                                             req <- uriToRequest url
                                             let addBasicAuth = applyBasicAuth (T.encodeUtf8 $ oauthClientId oa) (T.encodeUtf8 $ oauthClientSecret oa)
                                                 req' = (addBasicAuth . updateRequestHeaders Nothing) req
                                             httpLbs (urlEncodedBody body req') manager

--------------------------------------------------
-- * AUTH requests
--------------------------------------------------

-- | Conduct GET request and return response as JSON.
authGetJSON :: FromJSON a
                 => Manager                      -- ^ HTTP connection manager.
                 -> AccessToken
                 -> URI              -- ^ Full URL
                 -> IO (OAuth2Result a)          -- ^ Response as JSON
authGetJSON manager t uri = liftM parseResponseJSON $ authGetBS manager t uri

-- | Conduct GET request.
authGetBS :: Manager                              -- ^ HTTP connection manager.
             -> AccessToken
             -> URI                   -- ^ URL
             -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
authGetBS manager token url = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upReq = updateRequestHeaders (Just token) . setMethod HT.GET

-- | same to 'authGetBS' but set access token to query parameter rather than header
authGetBS' :: Manager                             -- ^ HTTP connection manager.
             -> AccessToken
             -> URI                               -- ^ URL
             -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
authGetBS' manager token url = do
  req <- uriToRequest (url `appendAccessToken` token)
  authRequest req upReq manager
  where upReq = updateRequestHeaders Nothing . setMethod HT.GET

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
             -> URI                   -- ^ URL
             -> PostBody
             -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
authPostBS manager token url pb = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upBody = urlEncodedBody (pb ++ accessTokenToParam token)
        upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
        upReq = upHeaders . upBody

-- | Conduct POST request with access token in the request body rather header
authPostBS' :: Manager                            -- ^ HTTP connection manager.
             -> AccessToken
             -> URI                               -- ^ URL
             -> PostBody
             -> IO (OAuth2Result BSL.ByteString)  -- ^ Response as ByteString
authPostBS' manager token url pb = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upBody = urlEncodedBody (pb ++ accessTokenToParam token)
        upHeaders = updateRequestHeaders Nothing . setMethod HT.POST
        upReq = upHeaders . upBody

-- |Send an HTTP request including the Authorization header with the specified
--  access token.
--
authRequest :: Request                          -- ^ Request to perform
               -> (Request -> Request)          -- ^ Modify request before sending
               -> Manager                       -- ^ HTTP connection manager.
               -> IO (OAuth2Result BSL.ByteString)
authRequest req upReq manager = liftM handleResponse (httpLbs (upReq req) manager)

--------------------------------------------------
-- * Utilities
--------------------------------------------------

-- | Parses a @Response@ to to @OAuth2Result@
handleResponse :: Response BSL.ByteString -> OAuth2Result BSL.ByteString
handleResponse rsp =
    if HT.statusIsSuccessful (responseStatus rsp)
        then Right $ responseBody rsp
        else Left $ BSL.append "hoauth2.HttpClient.handleResponse/Gaining token failed: " (responseBody rsp)

-- | Parses a @OAuth2Result BSL.ByteString@ into @FromJSON a => a@
parseResponseJSON :: FromJSON a
              => OAuth2Result BSL.ByteString
              -> OAuth2Result a
parseResponseJSON (Left b) = Left b
parseResponseJSON (Right b) = case decode b of
                            Nothing -> Left ("hoauth2.HttpClient.parseResponseJSON/Could not decode JSON: " `BSL.append` b)
                            Just x -> Right x

-- | Parses a @OAuth2Result BSL.ByteString@ that contains not JSON but a Query String
parseResponseString :: FromJSON a
              => OAuth2Result BSL.ByteString
              -> OAuth2Result a
parseResponseString (Left b) = Left b
parseResponseString (Right b) = case parseQuery $ BSL.toStrict b of
                              [] -> Left errorMessage
                              a -> case fromJSON $ queryToValue a of
                                    Error _ -> Left errorMessage
                                    Success x -> Right x
  where
    queryToValue = Object . HM.fromList . map paramToPair
    paramToPair (k, mv) = (T.decodeUtf8 k, maybe Null (String . T.decodeUtf8) mv)
    errorMessage = "hoauth2.HttpClient.parseResponseJSON/Could not decode JSON or URL: " `BSL.append` b

-- | Try 'parseResponseJSON' and 'parseResponseString'
parseResponseFlexible :: FromJSON a
                         => OAuth2Result BSL.ByteString
                         -> OAuth2Result a
parseResponseFlexible r = case parseResponseJSON r of
                           Left _ -> parseResponseString r
                           x -> x

-- | Set several header values:
--   + userAgennt    : `hoauth2`
--   + accept        : `application/json`
--   + authorization : 'Bearer' `xxxxx` if 'AccessToken' provided.
updateRequestHeaders :: Maybe AccessToken -> Request -> Request
updateRequestHeaders t req =
  let extras = [ (HT.hUserAgent, "hoauth2")
               , (HT.hAccept, "application/json") ]
      bearer = [(HT.hAuthorization, "Bearer " `BS.append` T.encodeUtf8 (fromJust (fmap atoken t))) | isJust t]
      headers = bearer ++ extras ++ requestHeaders req
  in
  req { requestHeaders = headers }

-- | Set the HTTP method to use.
setMethod :: HT.StdMethod -> Request -> Request
setMethod m req = req { method = HT.renderStdMethod m }
