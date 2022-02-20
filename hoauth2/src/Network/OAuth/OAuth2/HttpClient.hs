{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple http client to request OAuth2 tokens and several utils.
module Network.OAuth.OAuth2.HttpClient
  ( -- * AUTH requests
    authGetJSON,
    authGetBS,
    authGetBS2,
    authPostJSON,
    authPostBS,
    authPostBS2,
    authPostBS3,
    authRequest,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import qualified Data.Text.Encoding as T
import Lens.Micro
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import Network.OAuth.OAuth2.Internal
import URI.ByteString

--------------------------------------------------

-- * AUTH requests

-- Making request with Access Token appended to Header, Request body or query string.
--
--------------------------------------------------

-- | Conduct an authorized GET request and return response as JSON.
--   Inject Access Token to Authorization Header.
authGetJSON ::
  (FromJSON b) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString IO b
authGetJSON manager t uri = do
  resp <- authGetBS manager t uri
  case eitherDecode resp of
    Right obj -> return obj
    Left e -> throwE $ BSL.pack e

-- | Conduct an authorized GET request.
--   Inject Access Token to Authorization Header.
authGetBS ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBS manager token url = do
  req <- uriToRequest url
  authRequest req upReq manager
  where
    upReq = updateRequestHeaders (Just token) . setMethod HT.GET

-- | Same to 'authGetBS' but set access token to query parameter rather than header
authGetBS2 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBS2 manager token url = do
  req <- liftIO $ uriToRequest (url `appendAccessToken` token)
  authRequest req upReq manager
  where
    upReq = updateRequestHeaders Nothing . setMethod HT.GET

-- | Conduct POST request and return response as JSON.
--   Inject Access Token to Authorization Header and request body.
authPostJSON ::
  (FromJSON b) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as JSON
  ExceptT BSL.ByteString IO b
authPostJSON manager t uri pb = do
  resp <- authPostBS manager t uri pb
  case eitherDecode resp of
    Right obj -> return obj
    Left e -> throwE $ BSL.pack e

-- | Conduct POST request.
--   Inject Access Token to http header (Authorization) and request body.
authPostBS ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS manager token url pb = do
  req <- uriToRequest url
  authRequest req upReq manager
  where
    upBody = urlEncodedBody (pb ++ accessTokenToParam token)
    upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
    upReq = upHeaders . upBody

-- | Conduct POST request with access token only in the request body but header.
authPostBS2 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS2 manager token url pb = do
  req <- uriToRequest url
  authRequest req upReq manager
  where
    upBody = urlEncodedBody (pb ++ accessTokenToParam token)
    upHeaders = updateRequestHeaders Nothing . setMethod HT.POST
    upReq = upHeaders . upBody

-- | Conduct POST request with access token only in the header and not in body
authPostBS3 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS3 manager token url = do
  req <- uriToRequest url
  authRequest req upReq manager
  where
    upBody req = req {requestBody = "null"}
    upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST
    upReq = upHeaders . upBody

--------------------------------------------------

-- * Utilities

--------------------------------------------------

-- | Send an HTTP request.
authRequest ::
  -- | Request to perform
  Request ->
  -- | Modify request before sending
  (Request -> Request) ->
  -- | HTTP connection manager.
  Manager ->
  ExceptT BSL.ByteString IO BSL.ByteString
authRequest req upReq manage = ExceptT $ handleResponse <$> httpLbs (upReq req) manage

-- | Parses a @Response@ to to @OAuth2Result@
handleResponse :: Response BSL.ByteString -> Either BSL.ByteString BSL.ByteString
handleResponse rsp =
  if HT.statusIsSuccessful (responseStatus rsp)
    then Right $ responseBody rsp
    else -- TODO: better to surface up entire resp so that client can decide what to do when error happens.
      Left $ responseBody rsp

-- | Set several header values:
--   + userAgennt    : `hoauth2`
--   + accept        : `application/json`
--   + authorization : 'Bearer' `xxxxx` if 'AccessToken' provided.
updateRequestHeaders :: Maybe AccessToken -> Request -> Request
updateRequestHeaders t req =
  let bearer = [(HT.hAuthorization, "Bearer " `BS.append` T.encodeUtf8 (atoken (fromJust t))) | isJust t]
      headers = bearer ++ defaultRequestHeaders ++ requestHeaders req
   in req {requestHeaders = headers}

-- | Set the HTTP method to use.
setMethod :: HT.StdMethod -> Request -> Request
setMethod m req = req {method = HT.renderStdMethod m}

-- | For `GET` method API.
appendAccessToken ::
  -- | Base URI
  URIRef a ->
  -- | Authorized Access Token
  AccessToken ->
  -- | Combined Result
  URIRef a
appendAccessToken uri t = over (queryL . queryPairsL) (\query -> query ++ accessTokenToParam t) uri

-- | Create 'QueryParams' with given access token value.
accessTokenToParam :: AccessToken -> [(BS.ByteString, BS.ByteString)]
accessTokenToParam t = [("access_token", T.encodeUtf8 $ atoken t)]
