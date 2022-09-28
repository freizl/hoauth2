{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings for The OAuth 2.0 Authorization Framework: Bearer Token Usage
-- RFC6750 <https://www.rfc-editor.org/rfc/rfc6750>
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
    APIAuthenticationMethod (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Encoding as T
import Lens.Micro (over)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import Network.OAuth.OAuth2.Internal
import URI.ByteString (URI, URIRef, queryL, queryPairsL)

--------------------------------------------------

-- * AUTH requests

-- Making request with Access Token appended to Header, Request body or query string.
--
--------------------------------------------------

-- | Conduct an authorized GET request and return response as JSON.
--   Inject Access Token to Authorization Header.
authGetJSON ::
  (FromJSON a, MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString m a
authGetJSON = authGetJSONWithAuthMethod AuthInRequestHeader

authGetJSONInternal ::
  (FromJSON a, MonadIO m) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString m a
authGetJSONInternal = authGetJSONWithAuthMethod
{-# DEPRECATED authGetJSONInternal "use authGetJSONWithAuthMethod" #-}

-- | Conduct an authorized GET request and return response as JSON.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authGetJSONWithAuthMethod ::
  (MonadIO m, FromJSON a) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString m a
authGetJSONWithAuthMethod authTypes manager t uri = do
  resp <- authGetBSWithAuthMethod authTypes manager t uri
  either (throwE . BSL.pack) return (eitherDecode resp)

-- | Conduct an authorized GET request.
--   Inject Access Token to Authorization Header.
authGetBS ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authGetBS = authGetBSWithAuthMethod AuthInRequestHeader

-- | Same to 'authGetBS' but set access token to query parameter rather than header
authGetBS2 ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authGetBS2 = authGetBSWithAuthMethod AuthInRequestQuery
{-# DEPRECATED authGetBS2 "use authGetBSWithAuthMethod" #-}

authGetBSInternal ::
  (MonadIO m) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authGetBSInternal = authGetBSWithAuthMethod
{-# DEPRECATED authGetBSInternal "use authGetBSWithAuthMethod" #-}

-- | Conduct an authorized GET request and return response as ByteString.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authGetBSWithAuthMethod ::
  (MonadIO m) =>
  -- | Specify the way that how to append the 'AccessToken' in the request
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
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
  (FromJSON a, MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as JSON
  ExceptT BSL.ByteString m a
authPostJSON = authPostJSONWithAuthMethod AuthInRequestHeader

authPostJSONInternal ::
  (FromJSON a, MonadIO m) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m a
authPostJSONInternal = authPostJSONWithAuthMethod
{-# DEPRECATED authPostJSONInternal "use 'authPostJSONWithAuthMethod'" #-}

-- | Conduct POST request and return response as JSON.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authPostJSONWithAuthMethod ::
  (FromJSON a, MonadIO m) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m a
authPostJSONWithAuthMethod authTypes manager token url body = do
  resp <- authPostBSWithAuthMethod authTypes manager token url body
  either (throwE . BSL.pack) return (eitherDecode resp)

-- | Conduct POST request.
--   Inject Access Token to http header (Authorization)
authPostBS ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authPostBS = authPostBSWithAuthMethod AuthInRequestHeader

-- | Conduct POST request with access token only in the request body but header.
authPostBS2 ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authPostBS2 = authPostBSWithAuthMethod AuthInRequestBody
{-# DEPRECATED authPostBS2 "use 'authPostBSWithAuthMethod'" #-}

-- | Conduct POST request with access token only in the header and not in body
authPostBS3 ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authPostBS3 = authPostBSWithAuthMethod AuthInRequestHeader
{-# DEPRECATED authPostBS3 "use 'authPostBSWithAuthMethod'" #-}

authPostBSInternal ::
  (MonadIO m) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authPostBSInternal = authPostBSWithAuthMethod
{-# DEPRECATED authPostBSInternal "use 'authPostBSWithAuthMethod'" #-}

-- | Conduct POST request and return response as ByteString.
--   Allow to specify how to append AccessToken.
--
-- @since 2.6.0
authPostBSWithAuthMethod ::
  (MonadIO m) =>
  APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authPostBSWithAuthMethod authTypes manager token url body = do
  let appendToBody = AuthInRequestBody == authTypes
  let appendToHeader = AuthInRequestHeader == authTypes
  let reqBody = if appendToBody then body ++ accessTokenToParam token else body
  -- TODO: urlEncodedBody send request as 'application/x-www-form-urlencoded'
  -- seems shall go with application/json which is more common?
  let upBody = if null reqBody then id else urlEncodedBody reqBody
  let upHeaders = updateRequestHeaders (if appendToHeader then Just token else Nothing) . setMethod HT.POST
  let upReq = upHeaders . upBody

  req <- liftIO $ uriToRequest url
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
  (MonadIO m) =>
  -- | Request to perform
  Request ->
  -- | Modify request before sending
  (Request -> Request) ->
  -- | HTTP connection manager.
  Manager ->
  ExceptT BSL.ByteString m BSL.ByteString
authRequest req upReq manage = ExceptT $ handleResponse <$> httpLbs (upReq req) manage

-- | Get response body out of a @Response@
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
