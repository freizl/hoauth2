{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple http client to request OAuth2 tokens and several utils.

module Network.OAuth.OAuth2.HttpClient (
-- * Token management
  fetchAccessToken,
  fetchAccessToken2,
  refreshAccessToken,
  refreshAccessToken2,
-- * AUTH requests
  authGetJSON,
  authGetBS,
  authGetBS2,
  authPostJSON,
  authPostBS,
  authPostBS2,
  authPostBS3,
  authRequest
) where

import           Data.Aeson
import           Data.Bifunctor                    (first)
import qualified Data.ByteString.Char8             as BS
import qualified Data.ByteString.Lazy.Char8        as BSL
import qualified Data.HashMap.Strict               as HM (fromList)
import           Data.Maybe
import qualified Data.Text.Encoding                as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types                as HT
import           Network.HTTP.Types.URI            (parseQuery)
import           Network.OAuth.OAuth2.Internal
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           URI.ByteString

--------------------------------------------------
-- * Token management
--------------------------------------------------

-- | Fetch OAuth2 Token with authenticate in request header.
--
-- OAuth2 spec allows `client_id` and `client_secret` to
-- either be sent in the header (as basic authentication)
-- OR as form/url params.
-- The OAuth server can choose to implement only one, or both.
-- Unfortunately, there is no way for the OAuth client (i.e. this library) to
-- know which method to use. Please take a look at the documentation of the
-- service that you are integrating with and either use `fetchAccessToken` or `fetchAccessToken2`
fetchAccessToken :: Manager                                   -- ^ HTTP connection manager
                   -> OAuth2                                  -- ^ OAuth Data
                   -> ExchangeToken                           -- ^ OAuth2 Code
                   -> IO (OAuth2Result TR.Errors OAuth2Token) -- ^ Access Token
fetchAccessToken manager oa code = doJSONPostRequest manager oa uri body
                           where (uri, body) = accessTokenUrl oa code

-- | Please read the docs of `fetchAccessToken`.
--
fetchAccessToken2 :: Manager                                   -- ^ HTTP connection manager
                   -> OAuth2                                  -- ^ OAuth Data
                   -> ExchangeToken                           -- ^ OAuth 2 Tokens
                   -> IO (OAuth2Result TR.Errors OAuth2Token) -- ^ Access Token
fetchAccessToken2 mgr oa code = do
  let (url, body1) = accessTokenUrl oa code
  let secret x = [("client_secret", T.encodeUtf8 x)]
  let extraBody = ("client_id", T.encodeUtf8 $ oauthClientId oa) : maybe [] secret (oauthClientSecret oa)
  doJSONPostRequest mgr oa url (extraBody ++ body1)

-- | Fetch a new AccessToken with the Refresh Token with authentication in request header.
-- OAuth2 spec allows `client_id` and `client_secret` to
-- either be sent in the header (as basic authentication)
-- OR as form/url params.
-- The OAuth server can choose to implement only one, or both.
-- Unfortunately, there is no way for the OAuth client (i.e. this library) to
-- know which method to use. Please take a look at the documentation of the
-- service that you are integrating with and either use `refreshAccessToken` or `refreshAccessToken2`
refreshAccessToken :: Manager                         -- ^ HTTP connection manager.
                     -> OAuth2                       -- ^ OAuth context
                     -> RefreshToken                 -- ^ refresh token gained after authorization
                     -> IO (OAuth2Result TR.Errors OAuth2Token)
refreshAccessToken manager oa token = doJSONPostRequest manager oa uri body
                              where (uri, body) = refreshAccessTokenUrl oa token

-- | Please read the docs of `refreshAccessToken`.
--
refreshAccessToken2 :: Manager                         -- ^ HTTP connection manager.
                     -> OAuth2                       -- ^ OAuth context
                     -> RefreshToken                 -- ^ refresh token gained after authorization
                     -> IO (OAuth2Result TR.Errors OAuth2Token)
refreshAccessToken2 manager oa token = do
  let (uri, body) = refreshAccessTokenUrl oa token
  let secret x = [("client_secret", T.encodeUtf8 x)]
  let extraBody = ("client_id", T.encodeUtf8 $ oauthClientId oa) : maybe [] secret (oauthClientSecret oa)
  doJSONPostRequest manager oa uri (extraBody ++ body)

-- | Conduct post request and return response as JSON.
doJSONPostRequest :: (FromJSON err, FromJSON a)
                  => Manager                             -- ^ HTTP connection manager.
                  -> OAuth2                              -- ^ OAuth options
                  -> URI                                 -- ^ The URL
                  -> PostBody                            -- ^ request body
                  -> IO (OAuth2Result err a)             -- ^ Response as JSON
doJSONPostRequest manager oa uri body = fmap parseResponseFlexible (doSimplePostRequest manager oa uri body)

-- | Conduct post request.
doSimplePostRequest :: FromJSON err => Manager                 -- ^ HTTP connection manager.
                       -> OAuth2                               -- ^ OAuth options
                       -> URI                                  -- ^ URL
                       -> PostBody                             -- ^ Request body.
                       -> IO (OAuth2Result err BSL.ByteString) -- ^ Response as ByteString
doSimplePostRequest manager oa url body = fmap handleOAuth2TokenResponse go
                                  where go = do
                                             req <- uriToRequest url
                                             let addBasicAuth = case oauthClientSecret oa of
                                                   (Just secret) -> applyBasicAuth (T.encodeUtf8 $ oauthClientId oa) (T.encodeUtf8 secret)
                                                   Nothing -> id
                                                 req' = (addBasicAuth . updateRequestHeaders Nothing) req
                                             httpLbs (urlEncodedBody body req') manager

-- | Parses a @Response@ to to @OAuth2Result@
handleOAuth2TokenResponse :: FromJSON err => Response BSL.ByteString -> OAuth2Result err BSL.ByteString
handleOAuth2TokenResponse rsp =
    if HT.statusIsSuccessful (responseStatus rsp)
        then Right $ responseBody rsp
        else Left $ parseOAuth2Error (responseBody rsp)

-- | Try 'parseResponseJSON', if failed then parses the @OAuth2Result BSL.ByteString@ that contains not JSON but a Query String.
parseResponseFlexible :: FromJSON err => FromJSON a
                         => OAuth2Result err BSL.ByteString
                         -> OAuth2Result err a
parseResponseFlexible r = case parseResponseJSON r of
                           Left _ -> parseResponseString r
                           x      -> x

parseResponseJSON :: (FromJSON err, FromJSON a)
              => OAuth2Result err BSL.ByteString
              -> OAuth2Result err a
parseResponseJSON (Left b) = Left b
parseResponseJSON (Right b) = case eitherDecode b of
                            Left e  -> Left $ mkDecodeOAuth2Error b e
                            Right x -> Right x

-- | Parses a @OAuth2Result BSL.ByteString@ that contains not JSON but a Query String
parseResponseString :: (FromJSON err, FromJSON a)
              => OAuth2Result err BSL.ByteString
              -> OAuth2Result err a
parseResponseString (Left b) = Left b
parseResponseString (Right b) = case parseQuery $ BSL.toStrict b of
                              [] -> Left errorMessage
                              a -> case fromJSON $ queryToValue a of
                                    Error _   -> Left errorMessage
                                    Success x -> Right x
  where
    queryToValue = Object . HM.fromList . map paramToPair
    paramToPair (k, mv) = (T.decodeUtf8 k, maybe Null (String . T.decodeUtf8) mv)
    errorMessage = parseOAuth2Error b

--------------------------------------------------
-- * AUTH requests
--------------------------------------------------

-- | Conduct an authorized GET request and return response as JSON.
authGetJSON :: (FromJSON b)
                 => Manager                 -- ^ HTTP connection manager.
                 -> AccessToken
                 -> URI
                 -> IO (Either BSL.ByteString b) -- ^ Response as JSON
authGetJSON manager t uri = do
  resp <- authGetBS manager t uri
  return (resp >>= (first BSL.pack . eitherDecode))

-- | Conduct an authorized GET request.
authGetBS :: Manager                 -- ^ HTTP connection manager.
             -> AccessToken
             -> URI
             -> IO (Either BSL.ByteString BSL.ByteString) -- ^ Response as ByteString
authGetBS manager token url = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upReq = updateRequestHeaders (Just token) . setMethod HT.GET

-- | same to 'authGetBS' but set access token to query parameter rather than header
authGetBS2 :: Manager                -- ^ HTTP connection manager.
             -> AccessToken
             -> URI
             -> IO (Either BSL.ByteString BSL.ByteString) -- ^ Response as ByteString
authGetBS2 manager token url = do
  req <- uriToRequest (url `appendAccessToken` token)
  authRequest req upReq manager
  where upReq = updateRequestHeaders Nothing . setMethod HT.GET

-- | Conduct POST request and return response as JSON.
authPostJSON :: (FromJSON b)
                 => Manager                 -- ^ HTTP connection manager.
                 -> AccessToken
                 -> URI
                 -> PostBody
                 -> IO (Either BSL.ByteString b) -- ^ Response as JSON
authPostJSON manager t uri pb = do
  resp <- authPostBS manager t uri pb
  return (resp >>= (first BSL.pack . eitherDecode))

-- | Conduct POST request.
authPostBS :: Manager                -- ^ HTTP connection manager.
             -> AccessToken
             -> URI
             -> PostBody
             -> IO (Either BSL.ByteString BSL.ByteString) -- ^ Response as ByteString
authPostBS manager token url pb = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upBody = urlEncodedBody (pb ++ accessTokenToParam token)
        upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
        upReq = upHeaders . upBody

-- | Conduct POST request with access token in the request body rather header
authPostBS2 :: Manager               -- ^ HTTP connection manager.
             -> AccessToken
             -> URI
             -> PostBody
             -> IO (Either BSL.ByteString BSL.ByteString) -- ^ Response as ByteString
authPostBS2 manager token url pb = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upBody = urlEncodedBody (pb ++ accessTokenToParam token)
        upHeaders = updateRequestHeaders Nothing . setMethod HT.POST
        upReq = upHeaders . upBody

-- | Conduct POST request with access token in the header and null in body
authPostBS3 :: Manager               -- ^ HTTP connection manager.
             -> AccessToken
             -> URI
             -> IO (Either BSL.ByteString BSL.ByteString) -- ^ Response as ByteString
authPostBS3 manager token url = do
  req <- uriToRequest url
  authRequest req upReq manager
  where upBody req = req { requestBody = "null" }
        upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
        upReq = upHeaders . upBody

-- |Send an HTTP request including the Authorization header with the specified
--  access token.
--
authRequest :: Request          -- ^ Request to perform
               -> (Request -> Request)          -- ^ Modify request before sending
               -> Manager                       -- ^ HTTP connection manager.
               -> IO (Either BSL.ByteString BSL.ByteString)
authRequest req upReq manage = handleResponse <$> httpLbs (upReq req) manage

--------------------------------------------------
-- * Utilities
--------------------------------------------------

-- | Parses a @Response@ to to @OAuth2Result@
handleResponse :: Response BSL.ByteString -> Either BSL.ByteString BSL.ByteString
handleResponse rsp =
    if HT.statusIsSuccessful (responseStatus rsp)
        then Right $ responseBody rsp
        else Left $ responseBody rsp

-- | Set several header values:
--   + userAgennt    : `hoauth2`
--   + accept        : `application/json`
--   + authorization : 'Bearer' `xxxxx` if 'AccessToken' provided.
updateRequestHeaders :: Maybe AccessToken -> Request -> Request
updateRequestHeaders t req =
  let extras = [ (HT.hUserAgent, "hoauth2")
               , (HT.hAccept, "application/json") ]
      bearer = [(HT.hAuthorization, "Bearer " `BS.append` T.encodeUtf8 (atoken (fromJust t))) | isJust t]
      headers = bearer ++ extras ++ requestHeaders req
  in
  req { requestHeaders = headers }

-- | Set the HTTP method to use.
setMethod :: HT.StdMethod -> Request -> Request
setMethod m req = req { method = HT.renderStdMethod m }
