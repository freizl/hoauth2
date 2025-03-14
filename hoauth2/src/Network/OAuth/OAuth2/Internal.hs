{-# LANGUAGE QuasiQuotes #-}

module Network.OAuth.OAuth2.Internal where

import Control.Arrow (second)
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, explicitParseFieldMaybe)
import Data.Binary (Binary (..))
import Data.Binary.Instances.Aeson ()
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Default
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Version (showVersion)
import Lens.Micro
import Lens.Micro.Extras
import Network.HTTP.Conduit as C
import Network.HTTP.Types qualified as H
import Network.HTTP.Types qualified as HT
import Paths_hoauth2 (version)
import URI.ByteString
import URI.ByteString.Aeson ()
import URI.ByteString.QQ

-------------------------------------------------------------------------------

-- * OAuth2 Configuration

-------------------------------------------------------------------------------

-- | Query Parameter Representation
data OAuth2 = OAuth2
  { oauth2ClientId :: Text
  , oauth2ClientSecret :: Text
  , oauth2AuthorizeEndpoint :: URIRef Absolute
  , oauth2TokenEndpoint :: URIRef Absolute
  , oauth2RedirectUri :: URIRef Absolute
  }
  deriving (Show, Eq)

instance Default OAuth2 where
  def =
    OAuth2
      { oauth2ClientId = ""
      , oauth2ClientSecret = ""
      , oauth2AuthorizeEndpoint = [uri|https://www.example.com/|]
      , oauth2TokenEndpoint = [uri|https://www.example.com/|]
      , oauth2RedirectUri = [uri|https://www.example.com/|]
      }

-------------------------------------------------------------------------------

-- * Tokens

-------------------------------------------------------------------------------

newtype AccessToken = AccessToken {atoken :: Text} deriving (Eq, Show, FromJSON, ToJSON)

newtype RefreshToken = RefreshToken {rtoken :: Text} deriving (Eq, Show, FromJSON, ToJSON)

newtype IdToken = IdToken {idtoken :: Text} deriving (Eq, Show, FromJSON, ToJSON)

-- | Authorization Code
newtype ExchangeToken = ExchangeToken {extoken :: Text} deriving (Show, FromJSON, ToJSON)

-- FIXME: rename to TokenResponse and move to that module

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.4
data OAuth2Token = OAuth2Token
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

instance Show OAuth2Token where
  show OAuth2Token {..} =
    "OAuth2Token {"
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

instance Binary OAuth2Token where
  put OAuth2Token {..} = put rawResponse
  get = do
    rawt <- get
    case fromJSON (Aeson.Object rawt) of
      Success a -> pure a
      Error err -> fail err

-- | Parse JSON data into 'OAuth2Token'
instance FromJSON OAuth2Token where
  parseJSON :: Value -> Parser OAuth2Token
  parseJSON = withObject "OAuth2Token" $ \v ->
    OAuth2Token
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

instance ToJSON OAuth2Token where
  toJSON :: OAuth2Token -> Value
  toJSON = toJSON . Object . rawResponse
  toEncoding :: OAuth2Token -> Encoding
  toEncoding = toEncoding . Object . rawResponse

-------------------------------------------------------------------------------

-- * Client Authentication methods

-------------------------------------------------------------------------------

-- | How would the Client (RP) authenticate itself?
--
-- The client MUST NOT use more than one authentication method in each request.
-- Means use Authorization header or Post body.
--
-- See more details
--
-- https://www.rfc-editor.org/rfc/rfc6749#section-2.3
-- https://oauth.net/private-key-jwt/
-- https://www.rfc-editor.org/rfc/rfc7523.html
data ClientAuthenticationMethod
  = ClientSecretBasic
  | ClientSecretPost
  | ClientAssertionJwt
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- * Utilies for Request and URI

-------------------------------------------------------------------------------

-- | Type synonym of post body content
type PostBody = [(BS.ByteString, BS.ByteString)]

-- | Type sysnonym of request query params
type QueryParams = [(BS.ByteString, BS.ByteString)]

defaultRequestHeaders :: [(HT.HeaderName, BS.ByteString)]
defaultRequestHeaders =
  [ (HT.hUserAgent, "hoauth2-" <> BS8.pack (showVersion version))
  , (HT.hAccept, "application/json")
  ]

appendQueryParams :: [(BS.ByteString, BS.ByteString)] -> URIRef a -> URIRef a
appendQueryParams params =
  over (queryL . queryPairsL) (params ++)

-- TODO: why we need this method instead of `parseRequest`
-- https://hackage.haskell.org/package/http-client-0.7.18/docs/Network-HTTP-Client.html#v:parseRequest
--
uriToRequest :: MonadThrow m => URI -> m Request
uriToRequest auri = do
  ssl <- case view (uriSchemeL . schemeBSL) auri of
    "http" -> return False
    "https" -> return True
    s -> throwM $ InvalidUrlException (show auri) ("Invalid scheme: " ++ show s)
  let query = fmap (second Just) (view (queryL . queryPairsL) auri)
      hostL = authorityL . _Just . authorityHostL . hostBSL
      portL = authorityL . _Just . authorityPortL . _Just . portNumberL
      defaultPort = (if ssl then 443 else 80) :: Int

      req =
        setQueryString query $
          defaultRequest
            { secure = ssl
            , path = view pathL auri
            }
      req2 = (over hostLens . maybe id const . preview hostL) auri req
      req3 = (over portLens . (const . fromMaybe defaultPort) . preview portL) auri req2
  return req3

requestToUri :: Request -> URI
requestToUri req =
  URI
    ( Scheme
        ( if secure req
            then "https"
            else "http"
        )
    )
    (Just (Authority Nothing (Host $ host req) (Just $ Port $ port req)))
    (path req)
    (Query $ H.parseSimpleQuery $ queryString req)
    Nothing

hostLens :: Lens' Request BS.ByteString
hostLens f req = f (C.host req) <&> \h' -> req {C.host = h'}
{-# INLINE hostLens #-}

portLens :: Lens' Request Int
portLens f req = f (C.port req) <&> \p' -> req {C.port = p'}
{-# INLINE portLens #-}
