{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple http client to request OAuth2 tokens and several utils.
module Network.OAuth.OAuth2.HttpClient
  ( -- * AUTH requests
    authGetJSON,
    authGetBS,
    authGetBS2,
    authGetJSONInternal,
    authGetBSInternal,
    authPostJSON,
    authPostBS,
    authPostBS1,
    authPostBS2,
    authPostBS3,
    authPostJSONInternal,
    authPostBSInternal,
  )
where

import qualified Data.Set as Set
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
authGetJSON = authGetJSONInternal (Set.fromList [AuthInRequestHeader])
{-# DEPRECATED authGetJSON "use authGetJSONInternal" #-}

-- | Conduct an authorized GET request and return response as JSON.
--   Allow to specify how to append AccessToken.
authGetJSONInternal ::
  (FromJSON b) =>
  Set.Set APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString IO b
authGetJSONInternal authTypes manager t uri = do
  resp <- authGetBSInternal authTypes manager t uri
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
authGetBS = authGetBSInternal $ Set.fromList [AuthInRequestHeader]

-- | Same to 'authGetBS' but set access token to query parameter rather than header
authGetBS2 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBS2 = authGetBSInternal $ Set.fromList [AuthInRequestQuery]
{-# DEPRECATED authGetBS2 "use authGetBSInternal" #-}

-- | Conduct an authorized GET request and return response as ByteString.
--   Allow to specify how to append AccessToken.
authGetBSInternal ::
  -- |
  Set.Set APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authGetBSInternal authTypes manager token url = do
  let appendToUrl = AuthInRequestQuery `Set.member` authTypes
  let appendToHeader = AuthInRequestHeader `Set.member` authTypes
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
authPostJSON = authPostJSONInternal $ Set.fromList [AuthInRequestHeader]
{-# DEPRECATED authPostJSON "use authPostJSONInternal" #-}

-- | Conduct POST request and return response as JSON.
--   Allow to specify how to append AccessToken.
authPostJSONInternal ::
  FromJSON a =>
  Set.Set APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO a
authPostJSONInternal authTypes manager token url body = do
  resp <- authPostBSInternal authTypes manager token url body
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
authPostBS = authPostBSInternal $ Set.fromList [AuthInRequestHeader]

-- | Conduct POST request.
--   Inject Access Token to both http header (Authorization) and request body.
authPostBS1 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS1 = authPostBSInternal $ Set.fromList [AuthInRequestBody, AuthInRequestHeader]
{-# DEPRECATED authPostBS1 "use authPostBSInternal" #-}

-- | Conduct POST request with access token only in the request body but header.
authPostBS2 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS2 = authPostBSInternal $ Set.fromList [AuthInRequestBody]
{-# DEPRECATED authPostBS2 "use authPostBSInternal" #-}

-- | Conduct POST request with access token only in the header and not in body
authPostBS3 ::
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBS3 = authPostBSInternal $ Set.fromList [AuthInRequestHeader]
{-# DEPRECATED authPostBS3 "use authPostBSInternal" #-}

-- | Conduct POST request and return response as ByteString.
--   Allow to specify how to append AccessToken.
authPostBSInternal ::
  Set.Set APIAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as ByteString
  ExceptT BSL.ByteString IO BSL.ByteString
authPostBSInternal authTypes manager token url body = do
  let appendToBody = AuthInRequestBody `Set.member` authTypes
  let appendToHeader = AuthInRequestHeader `Set.member` authTypes
  let reqBody = if appendToBody then body ++ accessTokenToParam token else body
  -- TODO: urlEncodedBody send request as 'application/x-www-form-urlencoded'
  -- seems shall go with application/json which is more common?
  let upBody = if null reqBody then id else urlEncodedBody reqBody
  let upHeaders = updateRequestHeaders (if appendToHeader then Just token else Nothing) . setMethod HT.POST
  let upReq = upHeaders . upBody

  req <- uriToRequest url
  authRequest req upReq manager

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
