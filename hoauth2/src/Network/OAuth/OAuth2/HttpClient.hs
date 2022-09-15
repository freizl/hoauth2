{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings for The OAuth 2.0 Authorization Framework: Bearer Token Usage
-- RFC6750 <https://www.rfc-editor.org/rfc/rfc6750>
--
module Network.OAuth.OAuth2.HttpClient
  ( -- * AUTH requests
    authGetJSON,
    authGetBS,
    authGetBS2,
    authGetJSONWithAuthMethod,
    authGetJSONInternal,
    authGetBSWithAuthMethod,
    authGetBSInternal,
    authPostJSON,
    authPostBS,
    authPostBS2,
    authPostBS3,
    authPostJSONWithAuthMethod,
    authPostJSONInternal,
    authPostBSWithAuthMethod,
    authPostBSInternal,

    -- * Types
    APIAuthenticationMethod(..)
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
authGetJSON = authGetJSONWithAuthMethod AuthInRequestHeader

authGetJSONInternal ::
  (FromJSON b) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString IO b
authGetJSONInternal = authGetJSONWithAuthMethod
{-# DEPRECATED authGetJSONInternal "use authGetJSONWithAuthMethod" #-}

-- | Conduct an authorized GET request and return response as JSON.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authGetJSONWithAuthMethod ::
  (FromJSON b) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString IO b
authGetJSONWithAuthMethod authTypes manager t uri = do
  resp <- authGetBSWithAuthMethod authTypes manager t uri
  either (throwE . BSL.pack) return (eitherDecode resp)

-- | Conduct an authorized GET request.
--   Inject Access Token to Authorization Header.
authGetBS ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBS = authGetBSWithAuthMethod AuthInRequestHeader

-- | Same to 'authGetBS' but set access token to query parameter rather than header
authGetBS2 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBS2 = authGetBSWithAuthMethod AuthInRequestQuery
{-# DEPRECATED authGetBS2 "use authGetBSWithAuthMethod" #-}

authGetBSInternal ::
  -- |
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBSInternal = authGetBSWithAuthMethod
{-# DEPRECATED authGetBSInternal "use authGetBSWithAuthMethod" #-}

-- | Conduct an authorized GET request and return response as ByteString.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authGetBSWithAuthMethod ::
  -- | Specify the way that how to append the 'AccessToken' in the request
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBSWithAuthMethod authTypes manager token url = do
  let appendToUrl = AuthInRequestQuery == authTypes
  let appendToHeader = AuthInRequestHeader == authTypes
  let uri = if appendToUrl then url `appendAccessToken` token else url
  let upReq = updateRequestHeaders (if appendToHeader then Just token else Nothing) . setMethod HT.GET
  req <- liftIO $ uriToRequest uri
  authRequest req upReq manager

-- | Conduct POST request and return response as JSON.
--   Inject Access Token to Authorization Header.
authPostJSON ::
  (FromJSON b) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as JSON
  ExceptT BSL.ByteString IO b
authPostJSON = authPostJSONWithAuthMethod AuthInRequestHeader
{-# DEPRECATED authPostJSON "use 'authPostJSONWithAuthMethod'" #-}

authPostJSONInternal ::
  FromJSON a =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO a
authPostJSONInternal = authPostJSONWithAuthMethod
{-# DEPRECATED authPostJSONInternal "use 'authPostJSONWithAuthMethod'" #-}

-- | Conduct POST request and return response as JSON.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authPostJSONWithAuthMethod ::
  FromJSON a =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO a
authPostJSONWithAuthMethod authTypes manager token url body = do
  resp <- authPostBSWithAuthMethod authTypes manager token url body
  either (throwE . BSL.pack) return (eitherDecode resp)

-- | Conduct POST request.
--   Inject Access Token to http header (Authorization)
authPostBS ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS = authPostBSWithAuthMethod AuthInRequestHeader

-- | Conduct POST request with access token only in the request body but header.
authPostBS2 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS2 = authPostBSWithAuthMethod AuthInRequestBody
{-# DEPRECATED authPostBS2 "use 'authPostBSWithAuthMethod'" #-}

-- | Conduct POST request with access token only in the header and not in body
authPostBS3 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS3 = authPostBSWithAuthMethod AuthInRequestHeader
{-# DEPRECATED authPostBS3 "use 'authPostBSWithAuthMethod'" #-}

authPostBSInternal ::
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBSInternal = authPostBSWithAuthMethod
{-# DEPRECATED authPostBSInternal "use 'authPostBSWithAuthMethod'" #-}

-- | Conduct POST request and return response as ByteString.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authPostBSWithAuthMethod ::
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBSWithAuthMethod authTypes manager token url body = do
  let appendToBody = AuthInRequestBody == authTypes
  let appendToHeader = AuthInRequestHeader == authTypes
  let reqBody = if appendToBody then body ++ accessTokenToParam token else body
  -- TODO: urlEncodedBody send request as 'application/x-www-form-urlencoded'
  -- seems shall go with application/json which is more common?
  let upBody = if null reqBody then id else urlEncodedBody reqBody
  let upHeaders = updateRequestHeaders (if appendToHeader then Just token else Nothing) . setMethod HT.POST
  let upReq = upHeaders . upBody

  req <- uriToRequest url
  authRequest req upReq manager

--------------------------------------------------

-- * Types

--------------------------------------------------

-- | https://www.rfc-editor.org/rfc/rfc6750#section-2
data APIAuthenticationMethod
  = -- | Provides in Authorization header
    AuthInRequestHeader
  | -- | Provides in request body
    AuthInRequestBody
  | -- | Provides in request query parameter
    AuthInRequestQuery
  deriving (Eq, Ord)

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
