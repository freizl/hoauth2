-- | Bindings Access Token and Refresh Token part of The OAuth 2.0 Authorization Framework
-- RFC6749 <https://www.rfc-editor.org/rfc/rfc6749>
module Network.OAuth.OAuth2.TokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, explicitParseFieldMaybe)
import Data.Binary (Binary (..))
import Data.Binary.Instances.Aeson ()
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Network.HTTP.Conduit
import Network.HTTP.Types qualified as HT
import Network.HTTP.Types.URI (parseQuery)
import Network.OAuth.OAuth2.Internal
import URI.ByteString
import Prelude hiding (error)

--------------------------------------------------

-- * Token Request Errors

--------------------------------------------------

data TokenResponseError = TokenResponseError
  { tokenResponseError :: TokenResponseErrorCode
  , tokenResponseErrorDescription :: Maybe Text
  , tokenResponseErrorUri :: Maybe (URIRef Absolute)
  }
  deriving (Show, Eq)

-- | Token Error Responses https://tools.ietf.org/html/rfc6749#section-5.2
data TokenResponseErrorCode
  = InvalidRequest
  | InvalidClient
  | InvalidGrant
  | UnauthorizedClient
  | UnsupportedGrantType
  | InvalidScope
  | UnknownErrorCode Text
  deriving (Show, Eq)

instance FromJSON TokenResponseErrorCode where
  parseJSON = withText "parseJSON TokenResponseErrorCode" $ \t ->
    pure $ case t of
      "invalid_request" -> InvalidRequest
      "invalid_client" -> InvalidClient
      "invalid_grant" -> InvalidGrant
      "unauthorized_client" -> UnauthorizedClient
      "unsupported_grant_type" -> UnsupportedGrantType
      "invalid_scope" -> InvalidScope
      _ -> UnknownErrorCode t

instance FromJSON TokenResponseError where
  parseJSON = withObject "parseJSON TokenResponseError" $ \t -> do
    tokenResponseError <- t .: "error"
    tokenResponseErrorDescription <- t .:? "error_description"
    tokenResponseErrorUri <- t .:? "error_uri"
    pure TokenResponseError {..}

parseTokeResponseError :: BSL.ByteString -> TokenResponseError
parseTokeResponseError string =
  either (mkDecodeOAuth2Error string) id (eitherDecode string)
  where
    mkDecodeOAuth2Error :: BSL.ByteString -> String -> TokenResponseError
    mkDecodeOAuth2Error response err =
      TokenResponseError
        (UnknownErrorCode "")
        (Just $ T.pack $ "Decode TokenResponseError failed: " <> err <> "\n Original Response:\n" <> show (T.decodeUtf8 $ BSL.toStrict response))
        Nothing

-------------------------------------------------------------------------------

-- * Tokens

-------------------------------------------------------------------------------

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.4
data TokenResponse = TokenResponse
  { accessToken :: AccessToken
  , refreshToken :: Maybe RefreshToken
  -- ^ Exists when @offline_access@ scope is in the Authorization Request and the provider supports Refresh Access Token.
  , expiresIn :: Maybe Int
  , tokenType :: Maybe Text
  -- ^ See https://www.rfc-editor.org/rfc/rfc6749#section-5.1. It's required per spec. But OAuth2 provider implementation are vary. Maybe will remove 'Maybe' in future release.
  , idToken :: Maybe IdToken
  -- ^ Exists when @openid@ scope is in the Authorization Request and the provider supports OpenID protocol.
  , scope :: Maybe Text
  , rawResponse :: Object
  }
  deriving (Eq)

instance Show TokenResponse where
  show TokenResponse {..} =
    "TokenResponse {"
      <> "access_token = ***"
      <> ", id_token = "
      <> showM idToken
      <> ", refresh_token = "
      <> showM refreshToken
      <> ", expires_in = "
      <> show expiresIn
      <> ", token_type = "
      <> show tokenType
      <> ", scope = "
      <> show scope
      <> ", raw_response = ***"
      <> "}"
    where
      showM (Just _) = "***"
      showM Nothing = "Nothing"

instance Binary TokenResponse where
  put TokenResponse {..} = put rawResponse
  get = do
    rawt <- get
    case fromJSON (Aeson.Object rawt) of
      Success a -> pure a
      Error err -> fail err

-- | Parse JSON data into 'OAuth2Token'
instance FromJSON TokenResponse where
  parseJSON :: Value -> Parser TokenResponse
  parseJSON = withObject "TokenResponse" $ \v ->
    TokenResponse
      <$> v .: "access_token"
      <*> v .:? "refresh_token"
      <*> explicitParseFieldMaybe parseIntFlexible v "expires_in"
      <*> v .:? "token_type"
      <*> v .:? "id_token"
      <*> v .:? "scope"
      <*> pure v
    where
      parseIntFlexible :: Value -> Parser Int
      parseIntFlexible (String s) = pure . read $ unpack s
      parseIntFlexible v = parseJSON v

instance ToJSON TokenResponse where
  toJSON :: TokenResponse -> Value
  toJSON = toJSON . Object . rawResponse
  toEncoding :: TokenResponse -> Encoding
  toEncoding = toEncoding . Object . rawResponse

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
        [ ("code", T.encodeUtf8 $ extoken code)
        , ("redirect_uri", serializeURIRef' $ oauth2RedirectUri oa)
        , ("grant_type", "authorization_code")
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
      [ ("grant_type", "refresh_token")
      , ("refresh_token", T.encodeUtf8 $ rtoken token)
      ]

--------------------------------------------------

-- * Token management

--------------------------------------------------

-- | Exchange @code@ for an Access Token with authenticate in request header.
fetchAccessToken ::
  MonadIO m =>
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | OAuth2 Code
  ExchangeToken ->
  -- | Access Token
  ExceptT TokenResponseError m TokenResponse
fetchAccessToken = fetchAccessTokenWithAuthMethod ClientSecretBasic

-- | Exchange @code@ for an Access Token
--
-- OAuth2 spec allows credential (@client_id@, @client_secret@) to be sent
-- either in the header (a.k.a `ClientSecretBasic`).
-- or as form/url params (a.k.a `ClientSecretPost`).
--
-- The OAuth provider can choose to implement only one, or both.
-- Look for API document from the OAuth provider you're dealing with.
-- If you`re uncertain, try `fetchAccessToken` which sends credential
-- in authorization http header, which is common case.
--
-- @since 2.6.0
fetchAccessTokenWithAuthMethod ::
  MonadIO m =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager
  Manager ->
  -- | OAuth Data
  OAuth2 ->
  -- | Authorization Code
  ExchangeToken ->
  -- | Access Token
  ExceptT TokenResponseError m TokenResponse
fetchAccessTokenWithAuthMethod authMethod manager oa code = do
  let (uri, body) = accessTokenUrl oa code
  let extraBody = if authMethod == ClientSecretPost then clientSecretPost oa else []
  doJSONPostRequest manager oa uri (body ++ extraBody)

-- | Fetch a new AccessToken using the Refresh Token with authentication in request header.
refreshAccessToken ::
  MonadIO m =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | Refresh Token gained after authorization
  RefreshToken ->
  ExceptT TokenResponseError m TokenResponse
refreshAccessToken = refreshAccessTokenWithAuthMethod ClientSecretBasic

-- | Fetch a new AccessToken using the Refresh Token.
--
-- OAuth2 spec allows credential ("client_id", "client_secret") to be sent
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
  MonadIO m =>
  ClientAuthenticationMethod ->
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth context
  OAuth2 ->
  -- | Refresh Token gained after authorization
  RefreshToken ->
  ExceptT TokenResponseError m TokenResponse
refreshAccessTokenWithAuthMethod authMethod manager oa token = do
  let (uri, body) = refreshAccessTokenUrl oa token
  let extraBody = if authMethod == ClientSecretPost then clientSecretPost oa else []
  doJSONPostRequest manager oa uri (body ++ extraBody)

--------------------------------------------------

-- * Utilies

--------------------------------------------------

-- | Conduct post request and return response as JSON.
doJSONPostRequest ::
  (MonadIO m, FromJSON a) =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | The URL
  URI ->
  -- | request body
  PostBody ->
  -- | Response as JSON
  ExceptT TokenResponseError m a
doJSONPostRequest manager oa uri body = do
  resp <- doSimplePostRequest manager oa uri body
  case parseResponseFlexible resp of
    Right obj -> return obj
    Left e -> throwE e

-- | Conduct post request.
doSimplePostRequest ::
  MonadIO m =>
  -- | HTTP connection manager.
  Manager ->
  -- | OAuth options
  OAuth2 ->
  -- | URL
  URI ->
  -- | Request body.
  PostBody ->
  -- | Response as ByteString
  ExceptT TokenResponseError m BSL.ByteString
doSimplePostRequest manager oa url body =
  ExceptT . liftIO $ fmap handleOAuth2TokenResponse go
  where
    go = do
      req <- uriToRequest url
      let req' = (addBasicAuth oa . addDefaultRequestHeaders) req
      httpLbs (urlEncodedBody body req') manager

-- | Gets response body from a @Response@ if 200 otherwise assume 'Network.OAuth.OAuth2.TokenRequest.TokenResponseError'
handleOAuth2TokenResponse :: Response BSL.ByteString -> Either TokenResponseError BSL.ByteString
handleOAuth2TokenResponse rsp =
  if HT.statusIsSuccessful (responseStatus rsp)
    then Right $ responseBody rsp
    else Left $ parseTokeResponseError (responseBody rsp)

-- | Try to parses response as JSON, if failed, try to parse as like query string.
parseResponseFlexible ::
  FromJSON a =>
  BSL.ByteString ->
  Either TokenResponseError a
parseResponseFlexible r = case eitherDecode r of
  Left _ -> parseResponseString r
  Right x -> Right x

-- | Parses the response that contains not JSON but a Query String
parseResponseString ::
  FromJSON a =>
  BSL.ByteString ->
  Either TokenResponseError a
parseResponseString b = case parseQuery $ BSL.toStrict b of
  [] -> Left errorMessage
  a -> case fromJSON $ queryToValue a of
    Error _ -> Left errorMessage
    Success x -> Right x
  where
    queryToValue = Object . KeyMap.fromList . map paramToPair
    paramToPair (k, mv) = (Key.fromText $ T.decodeUtf8 k, maybe Null (String . T.decodeUtf8) mv)
    errorMessage = parseTokeResponseError b

-- | Add Basic Authentication header using client_id and client_secret.
addBasicAuth :: OAuth2 -> Request -> Request
addBasicAuth oa =
  applyBasicAuth
    (T.encodeUtf8 $ oauth2ClientId oa)
    (T.encodeUtf8 $ oauth2ClientSecret oa)

-- | Set several header values:
--   + userAgennt    : "hoauth2"
--   + accept        : "application/json"
addDefaultRequestHeaders :: Request -> Request
addDefaultRequestHeaders req =
  let headers = defaultRequestHeaders ++ requestHeaders req
   in req {requestHeaders = headers}

-- | Add Credential (client_id, client_secret) to the request post body.
clientSecretPost :: OAuth2 -> PostBody
clientSecretPost oa =
  [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa)
  , ("client_secret", T.encodeUtf8 $ oauth2ClientSecret oa)
  ]
