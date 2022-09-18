{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings Access Token and Refresh Token part of The OAuth 2.0 Authorization Framework
-- RFC6749 <https://www.rfc-editor.org/rfc/rfc6749>
module Network.OAuth.OAuth2.TokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.URI (parseQuery)
import Network.OAuth.OAuth2.Internal
import URI.ByteString (URI, serializeURIRef')

--------------------------------------------------

-- * Token Request Errors

--------------------------------------------------

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}

instance ToJSON Errors where
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}

-- | Token Error Responses https://tools.ietf.org/html/rfc6749#section-5.2
data Errors
  = InvalidRequest
  | InvalidClient
  | InvalidGrant
  | UnauthorizedClient
  | UnsupportedGrantType
  | InvalidScope
  deriving (Show, Eq, Generic)

--------------------------------------------------

-- * URL

--------------------------------------------------

-- | Prepare the URL and the request body query for fetching an access token.
accessTokenUrl ::
  OAuth2 ->
  -- | access code gained via authorization URL
  ExchangeToken ->
  -- | access token request URL plus the request body.
  (URI, PostBody)
accessTokenUrl oa code =
  let uri = oauth2TokenEndpoint oa
      body =
        [ ("code", T.encodeUtf8 $ extoken code),
          ("redirect_uri", serializeURIRef' $ oauth2RedirectUri oa),
          ("grant_type", "authorization_code")
        ]
   in (uri, body)

-- | Obtain a new access token by sending a Refresh Token to the Authorization server.
refreshAccessTokenUrl ::
  OAuth2 ->
  -- | Refresh Token gained via authorization URL
  RefreshToken ->
  -- | Refresh Token request URL plus the request body.
  (URI, PostBody)
refreshAccessTokenUrl oa token = (uri, body)
  where
    uri = oauth2TokenEndpoint oa
    body =
      [ ("grant_type", "refresh_token"),
        ("refresh_token", T.encodeUtf8 $ rtoken token)
      ]

--------------------------------------------------

-- * Token management

--------------------------------------------------

-- | Exchange @code@ for an Access Token with authenticate in request header.
fetchAccessToken ::
  (MonadIO m) =>
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | OAuth2 Code
  ExchangeToken ->
  -- | Access Token
  ExceptT (OAuth2Error Errors) m OAuth2Token
fetchAccessToken = fetchAccessTokenWithAuthMethod ClientSecretBasic

fetchAccessToken2 ::
  (MonadIO m) =>
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | Authorization Code
  ExchangeToken ->
  -- | Access Token
  ExceptT (OAuth2Error Errors) m OAuth2Token
fetchAccessToken2 = fetchAccessTokenWithAuthMethod ClientSecretPost
{-# DEPRECATED fetchAccessToken2 "use 'fetchAccessTokenWithAuthMethod'" #-}

fetchAccessTokenInternal ::
  (MonadIO m) =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | Authorization Code
  ExchangeToken ->
  -- | Access Token
  ExceptT (OAuth2Error Errors) m OAuth2Token
fetchAccessTokenInternal = fetchAccessTokenWithAuthMethod
{-# DEPRECATED fetchAccessTokenInternal "use 'fetchAccessTokenWithAuthMethod'" #-}

-- | Exchange @code@ for an Access Token
--
-- OAuth2 spec allows credential (`client_id`, `client_secret`) to be sent
-- either in the header (a.k.a 'ClientSecretBasic').
-- or as form/url params (a.k.a 'ClientSecretPost').
--
-- The OAuth provider can choose to implement only one, or both.
-- Look for API document from the OAuth provider you're dealing with.
-- If you're uncertain, try 'fetchAccessToken' which sends credential
-- in authorization http header, which is common case.
--
-- @since 2.6.0
fetchAccessTokenWithAuthMethod ::
  (MonadIO m) =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | Authorization Code
  ExchangeToken ->
  -- | Access Token
  ExceptT (OAuth2Error Errors) m OAuth2Token
fetchAccessTokenWithAuthMethod authMethod manager oa code = do
  let (uri, body) = accessTokenUrl oa code
  let extraBody = if authMethod == ClientSecretPost then clientSecretPost oa else []
  doJSONPostRequest manager oa uri (body ++ extraBody)

-- | Fetch a new AccessToken using the Refresh Token with authentication in request header.
refreshAccessToken ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | Refresh Token gained after authorization
  RefreshToken ->
  ExceptT (OAuth2Error Errors) m OAuth2Token
refreshAccessToken = refreshAccessTokenWithAuthMethod ClientSecretBasic

refreshAccessToken2 ::
  (MonadIO m) =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | Refresh Token gained after authorization
  RefreshToken ->
  ExceptT (OAuth2Error Errors) m OAuth2Token
refreshAccessToken2 = refreshAccessTokenWithAuthMethod ClientSecretPost
{-# DEPRECATED refreshAccessToken2 "use 'refreshAccessTokenWithAuthMethod'" #-}

refreshAccessTokenInternal ::
  (MonadIO m) =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | Refresh Token gained after authorization
  RefreshToken ->
  ExceptT (OAuth2Error Errors) m OAuth2Token
refreshAccessTokenInternal = refreshAccessTokenWithAuthMethod
{-# DEPRECATED refreshAccessTokenInternal "use 'refreshAccessTokenWithAuthMethod'" #-}

-- | Fetch a new AccessToken using the Refresh Token.
--
-- OAuth2 spec allows credential (`client_id`, `client_secret`) to be sent
-- either in the header (a.k.a 'ClientSecretBasic').
-- or as form/url params (a.k.a 'ClientSecretPost').
--
-- The OAuth provider can choose to implement only one, or both.
-- Look for API document from the OAuth provider you're dealing with.
-- If you're uncertain, try 'refreshAccessToken' which sends credential
-- in authorization http header, which is common case.
--
-- @since 2.6.0
refreshAccessTokenWithAuthMethod ::
  (MonadIO m) =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | Refresh Token gained after authorization
  RefreshToken ->
  ExceptT (OAuth2Error Errors) m OAuth2Token
refreshAccessTokenWithAuthMethod authMethod manager oa token = do
  let (uri, body) = refreshAccessTokenUrl oa token
  let extraBody = if authMethod == ClientSecretPost then clientSecretPost oa else []
  doJSONPostRequest manager oa uri (body ++ extraBody)

--------------------------------------------------

-- * Utilies

--------------------------------------------------

-- | Conduct post request and return response as JSON.
doJSONPostRequest ::
  (MonadIO m, FromJSON err, FromJSON a) =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | The URL
  URI ->
  -- | request body
  PostBody ->
  -- | Response as JSON
  ExceptT (OAuth2Error err) m a
doJSONPostRequest manager oa uri body = do
  resp <- doSimplePostRequest manager oa uri body
  case parseResponseFlexible resp of
    Right obj -> return obj
    Left e -> throwE e

-- | Conduct post request.
doSimplePostRequest ::
  (MonadIO m, FromJSON err) =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | URL
  URI ->
  -- | Request body.
  PostBody ->
  -- | Response as ByteString
  ExceptT (OAuth2Error err) m BSL.ByteString
doSimplePostRequest manager oa url body =
  ExceptT . liftIO $ fmap handleOAuth2TokenResponse go
  where
    addBasicAuth = applyBasicAuth (T.encodeUtf8 $ oauth2ClientId oa) (T.encodeUtf8 $ oauth2ClientSecret oa)
    go = do
      req <- uriToRequest url
      let req' = (addBasicAuth . addDefaultRequestHeaders) req
      httpLbs (urlEncodedBody body req') manager

-- | Gets response body from a @Response@ if 200 otherwise assume 'OAuth2Error'
handleOAuth2TokenResponse :: FromJSON err => Response BSL.ByteString -> Either (OAuth2Error err) BSL.ByteString
handleOAuth2TokenResponse rsp =
  if HT.statusIsSuccessful (responseStatus rsp)
    then Right $ responseBody rsp
    else Left $ parseOAuth2Error (responseBody rsp)

-- | Try to parses response as JSON, if failed, try to parse as like query string.
parseResponseFlexible ::
  (FromJSON err, FromJSON a) =>
  BSL.ByteString ->
  Either (OAuth2Error err) a
parseResponseFlexible r = case eitherDecode r of
  Left _ -> parseResponseString r
  Right x -> Right x

-- | Parses the response that contains not JSON but a Query String
parseResponseString ::
  (FromJSON err, FromJSON a) =>
  BSL.ByteString ->
  Either (OAuth2Error err) a
parseResponseString b = case parseQuery $ BSL.toStrict b of
  [] -> Left errorMessage
  a -> case fromJSON $ queryToValue a of
    Error _ -> Left errorMessage
    Success x -> Right x
  where
    queryToValue = Object . KeyMap.fromList . map paramToPair
    paramToPair (k, mv) = (Key.fromText $ T.decodeUtf8 k, maybe Null (String . T.decodeUtf8) mv)
    errorMessage = parseOAuth2Error b

-- | Set several header values:
--   + userAgennt    : `hoauth2`
--   + accept        : `application/json`
addDefaultRequestHeaders :: Request -> Request
addDefaultRequestHeaders req =
  let headers = defaultRequestHeaders ++ requestHeaders req
   in req {requestHeaders = headers}

-- | Add Credential (client_id, client_secret) to the request post body.
clientSecretPost :: OAuth2 -> PostBody
clientSecretPost oa =
  [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa),
    ("client_secret", T.encodeUtf8 $ oauth2ClientSecret oa)
  ]
