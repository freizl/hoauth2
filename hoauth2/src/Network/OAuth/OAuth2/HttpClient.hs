-- | Bindings for The OAuth 2.0 Authorization Framework: Bearer Token Usage
-- RFC6750 <https://www.rfc-editor.org/rfc/rfc6750>
module Network.OAuth.OAuth2.HttpClient (
  -- * AUTH requests
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
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text.Encoding qualified as T
import Lens.Micro (over)
import Network.HTTP.Client.Contrib (handleResponse)
import Network.HTTP.Conduit
import Network.HTTP.Types qualified as HT
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
  (MonadIO m, FromJSON a) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as JSON
  ExceptT BSL.ByteString m a
authGetJSON = authGetJSONWithAuthMethod AuthInRequestHeader

-- | Deprecated. Use `authGetJSONWithAuthMethod` instead.
authGetJSONInternal ::
  (MonadIO m, FromJSON a) =>
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
  either (throwE . BSL.pack) return (Aeson.eitherDecode resp)

-- | Conduct an authorized GET request.
--   Inject Access Token to Authorization Header.
authGetBS ::
  MonadIO m =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authGetBS = authGetBSWithAuthMethod AuthInRequestHeader

-- | Same to 'authGetBS' but set access token to query parameter rather than header
authGetBS2 ::
  MonadIO m =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  -- | Response as ByteString
  ExceptT BSL.ByteString m BSL.ByteString
authGetBS2 = authGetBSWithAuthMethod AuthInRequestQuery
{-# DEPRECATED authGetBS2 "use authGetBSWithAuthMethod" #-}

authGetBSInternal ::
  MonadIO m =>
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
  MonadIO m =>
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
  (MonadIO m, FromJSON a) =>
  -- | HTTP connection manager.
  Manager ->
  AccessToken ->
  URI ->
  PostBody ->
  -- | Response as JSON
  ExceptT BSL.ByteString m a
authPostJSON = authPostJSONWithAuthMethod AuthInRequestHeader

authPostJSONInternal ::
  (MonadIO m, FromJSON a) =>
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
  (MonadIO m, FromJSON a) =>
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
  either (throwE . BSL.pack) return (Aeson.eitherDecode resp)

-- | Conduct POST request.
--   Inject Access Token to http header (Authorization)
authPostBS ::
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  let upBody = if null reqBody then id else jsonBody reqBody
  let upHeaders = updateRequestHeaders (if appendToHeader then Just token else Nothing) . setMethod HT.POST
  let upReq = upHeaders . upBody

  req <- liftIO $ uriToRequest url
  authRequest req upReq manager

jsonBody :: PostBody -> Request -> Request
jsonBody body req =
  req
    { requestBody =
        RequestBodyLBS $
          Aeson.encode $
            Aeson.fromList $
              fmap (\(a, b) -> (Aeson.fromText (T.decodeUtf8 a), T.decodeUtf8 b)) body
    , requestHeaders =
        (HT.hContentType, "application/json")
          : filter (\(x, _) -> x /= HT.hContentType) (requestHeaders req)
    }

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
  MonadIO m =>
  -- | Request to perform
  Request ->
  -- | Modify request before sending
  (Request -> Request) ->
  -- | HTTP connection manager.
  Manager ->
  ExceptT BSL.ByteString m BSL.ByteString
authRequest req upReq manage = ExceptT $ do
  resp <- httpLbs (upReq req) manage
  pure (handleResponse resp)

-- | Set several header values:
--   + userAgennt    : "hoauth2"
--   + authorization : "Bearer xxxxx" if 'Network.OAuth.OAuth2.AccessToken' provided.
updateRequestHeaders :: Maybe AccessToken -> Request -> Request
updateRequestHeaders t req =
  let bearer = case t of
        Just (AccessToken at) -> [(HT.hAuthorization, "Bearer " `BS.append` T.encodeUtf8 at)]
        Nothing -> []
      headers = bearer ++ defaultRequestHeaders ++ requestHeaders req
   in req {requestHeaders = headers}

-- | Set the HTTP method to use.
setMethod :: HT.StdMethod -> Request -> Request
setMethod m req = req {method = HT.renderStdMethod m}

-- | For @GET@ method API.
appendAccessToken ::
  -- | Base URI
  URIRef a ->
  -- | Authorized Access Token
  AccessToken ->
  -- | Combined Result
  URIRef a
appendAccessToken uri t = over (queryL . queryPairsL) (\query -> query ++ accessTokenToParam t) uri

-- | Create `QueryParams` with given access token value.
accessTokenToParam :: AccessToken -> [(BS.ByteString, BS.ByteString)]
accessTokenToParam t = [("access_token", T.encodeUtf8 $ atoken t)]
