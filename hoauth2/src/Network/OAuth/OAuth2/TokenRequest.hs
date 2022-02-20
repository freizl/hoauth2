{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.OAuth.OAuth2.TokenRequest where

import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as HT
import Network.HTTP.Types.URI (parseQuery)
import Network.OAuth.OAuth2.Internal
import URI.ByteString

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
accessTokenUrl oa code = accessTokenUrl' oa code (Just "authorization_code")

-- | Prepare the URL and the request body query for fetching an access token.
-- also include client_id and client_secret in the post body
accessTokenUrlClientCredInBody ::
  OAuth2 ->
  -- | access code gained via authorization URL
  ExchangeToken ->
  -- | access token request URL plus the request body.
  (URI, PostBody)
accessTokenUrlClientCredInBody oa code =
  let (uri, body) = accessTokenUrl oa code
      creds =
        [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa),
          ("client_secret", T.encodeUtf8 $ oauth2ClientSecret oa)
        ]
   in (uri, body ++ creds)

-- | Prepare the URL and the request body query for fetching an access token, with
-- optional grant type.
accessTokenUrl' ::
  OAuth2 ->
  -- | access code gained via authorization URL
  ExchangeToken ->
  -- | Grant Type
  Maybe Text ->
  -- | access token request URL plus the request body.
  (URI, PostBody)
accessTokenUrl' oa code gt = (uri, body)
  where
    uri = oauth2TokenEndpoint oa
    body =
      catMaybes
        [ Just ("code", T.encodeUtf8 $ extoken code),
          ("redirect_uri",) . serializeURIRef' <$> oauth2RedirectUri oa,
          fmap (("grant_type",) . T.encodeUtf8) gt
        ]

-- | Using a Refresh Token.  Obtain a new access token by
-- sending a refresh token to the Authorization server.
refreshAccessTokenUrl ::
  OAuth2 ->
  -- | refresh token gained via authorization URL
  RefreshToken ->
  -- | refresh token request URL plus the request body.
  (URI, PostBody)
refreshAccessTokenUrl oa token = (uri, body)
  where
    uri = oauth2TokenEndpoint oa
    body =
      [ ("grant_type", "refresh_token"),
        ("refresh_token", T.encodeUtf8 $ rtoken token)
      ]

refreshAccessTokenUrlClientCredInBody ::
  OAuth2 ->
  -- | refresh token gained via authorization URL
  RefreshToken ->
  -- | refresh token request URL plus the request body.
  (URI, PostBody)
refreshAccessTokenUrlClientCredInBody oa token =
  let (uri, body) = refreshAccessTokenUrl oa token
      creds =
        [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa),
          ("client_secret", T.encodeUtf8 $ oauth2ClientSecret oa)
        ]
   in (uri, body ++ creds)

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
-- know which method to use.
-- Please take a look at the documentation of the
-- service that you are integrating with and either use `fetchAccessToken` or `fetchAccessToken2`
fetchAccessToken ::
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | OAuth2 Code
  ExchangeToken ->
  -- | Access Token
  ExceptT (OAuth2Error Errors) IO OAuth2Token
fetchAccessToken manager oa code = doJSONPostRequest manager oa uri body
  where
    (uri, body) = accessTokenUrl oa code

-- | Fetch OAuth2 Token with authenticate in both request header and body.
-- Please read the docs of `fetchAccessToken`.
fetchAccessToken2 ::
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | OAuth 2 Tokens
  ExchangeToken ->
  -- | Access Token
  ExceptT (OAuth2Error Errors) IO OAuth2Token
fetchAccessToken2 manager oa code = doJSONPostRequest manager oa uri body
  where
    (uri, body) = accessTokenUrlClientCredInBody oa code

-- | Fetch a new AccessToken with the Refresh Token with authentication in request header.
--
-- OAuth2 spec allows `client_id` and `client_secret` to
-- either be sent in the header (as basic authentication)
-- OR as form/url params.
-- The OAuth server can choose to implement only one, or both.
-- Unfortunately, there is no way for the OAuth client (i.e. this library) to
-- know which method to use. Please take a look at the documentation of the
-- service that you are integrating with and either use `refreshAccessToken` or `refreshAccessToken2`
refreshAccessToken ::
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | refresh token gained after authorization
  RefreshToken ->
  ExceptT (OAuth2Error Errors) IO OAuth2Token
refreshAccessToken manager oa token = doJSONPostRequest manager oa uri body
  where
    (uri, body) = refreshAccessTokenUrl oa token

-- | Fetch a new AccessToken with the Refresh Token with authentication in request header and request body.
-- Please read the docs of `refreshAccessToken`.
--
refreshAccessToken2 ::
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | refresh token gained after authorization
  RefreshToken ->
  ExceptT (OAuth2Error Errors) IO OAuth2Token
refreshAccessToken2 manager oa token = doJSONPostRequest manager oa uri body
  where
    (uri, body) = refreshAccessTokenUrlClientCredInBody oa token

--------------------------------------------------

-- * Utilies

--------------------------------------------------

-- | Conduct post request and return response as JSON.
doJSONPostRequest ::
  (FromJSON err, FromJSON a) =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | The URL
  URI ->
  -- | request body
  PostBody ->
  -- | Response as JSON
  ExceptT (OAuth2Error err) IO a
doJSONPostRequest manager oa uri body = do
  resp <- doSimplePostRequest manager oa uri body
  case parseResponseFlexible resp of
    Right obj -> return obj
    Left e -> throwE e

-- | Conduct post request.
doSimplePostRequest ::
  FromJSON err =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | URL
  URI ->
  -- | Request body.
  PostBody ->
  -- | Response as ByteString
  ExceptT (OAuth2Error err) IO BSL.ByteString
doSimplePostRequest manager oa url body =
  ExceptT $ fmap handleOAuth2TokenResponse go
  where
    addBasicAuth = applyBasicAuth (T.encodeUtf8 $ oauth2ClientId oa) (T.encodeUtf8 $ oauth2ClientSecret oa)
    go = do
      req <- uriToRequest url
      let req' = (addBasicAuth . addDefaultRequestHeaders) req
      httpLbs (urlEncodedBody body req') manager

-- | Parses a @Response@ to to @OAuth2Result@
handleOAuth2TokenResponse :: FromJSON err => Response BSL.ByteString -> Either (OAuth2Error err) BSL.ByteString
handleOAuth2TokenResponse rsp =
  if HT.statusIsSuccessful (responseStatus rsp)
    then Right $ responseBody rsp
    else Left $ parseOAuth2Error (responseBody rsp)

-- | Try 'parseResponseJSON', if failed then parses the @OAuth2Result BSL.ByteString@ that contains not JSON but a Query String.
parseResponseFlexible ::
  (FromJSON err, FromJSON a) =>
  BSL.ByteString ->
  Either (OAuth2Error err) a
parseResponseFlexible r = case eitherDecode r of
  Left _ -> parseResponseString r
  Right x -> Right x

-- | Parses a @OAuth2Result BSL.ByteString@ that contains not JSON but a Query String
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
