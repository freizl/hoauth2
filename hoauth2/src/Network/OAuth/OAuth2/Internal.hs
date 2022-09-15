{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK -ignore-exports #-}

-- | A simple OAuth2 Haskell binding.  (This is supposed to be
-- independent of the http client used.)
module Network.OAuth.OAuth2.Internal where

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Parser, explicitParseFieldMaybe)
import Data.Binary (Binary)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import GHC.Generics
import Lens.Micro
import Lens.Micro.Extras
import Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types as HT
import URI.ByteString
import URI.ByteString.Aeson ()
import URI.ByteString.QQ

--------------------------------------------------

-- * Data Types

--------------------------------------------------

-- | Query Parameter Representation
data OAuth2 = OAuth2
  { oauth2ClientId :: Text,
    oauth2ClientSecret :: Text,
    oauth2AuthorizeEndpoint :: URIRef Absolute,
    oauth2TokenEndpoint :: URIRef Absolute,
    oauth2RedirectUri :: URIRef Absolute
  }
  deriving (Show, Eq)

instance Default OAuth2 where
  def =
    OAuth2
      { oauth2ClientId = "",
        oauth2ClientSecret = "",
        oauth2AuthorizeEndpoint = [uri|https://www.example.com/|],
        oauth2TokenEndpoint = [uri|https://www.example.com/|],
        oauth2RedirectUri = [uri|https://www.example.com/|]
      }

newtype AccessToken = AccessToken {atoken :: Text} deriving (Binary, Eq, Show, FromJSON, ToJSON)

newtype RefreshToken = RefreshToken {rtoken :: Text} deriving (Binary, Eq, Show, FromJSON, ToJSON)

newtype IdToken = IdToken {idtoken :: Text} deriving (Binary, Eq, Show, FromJSON, ToJSON)

newtype ExchangeToken = ExchangeToken {extoken :: Text} deriving (Show, FromJSON, ToJSON)

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.4
data OAuth2Token = OAuth2Token
  { accessToken :: AccessToken,
    refreshToken :: Maybe RefreshToken,
    expiresIn :: Maybe Int,
    tokenType :: Maybe Text,
    -- This is really not part of RFC6749 but OIDC Spec. A bit hacky.
    idToken :: Maybe IdToken
  }
  deriving (Eq, Show, Generic)

instance Binary OAuth2Token

-- | Parse JSON data into 'OAuth2Token'
instance FromJSON OAuth2Token where
  parseJSON = withObject "OAuth2Token" $ \v ->
    OAuth2Token
      <$> v .: "access_token"
      <*> v .:? "refresh_token"
      <*> explicitParseFieldMaybe parseIntFlexible v "expires_in"
      <*> v .:? "token_type"
      <*> v .:? "id_token"
    where
      parseIntFlexible :: Value -> Parser Int
      parseIntFlexible (String s) = pure . read $ unpack s
      parseIntFlexible v = parseJSON v

instance ToJSON OAuth2Token where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = camelTo2 '_'}

data OAuth2Error a = OAuth2Error
  { error :: Either Text a,
    errorDescription :: Maybe Text,
    errorUri :: Maybe (URIRef Absolute)
  }
  deriving (Show, Eq, Generic)

instance FromJSON err => FromJSON (OAuth2Error err) where
  parseJSON (Object a) =
    do
      err <- (a .: "error") >>= (\str -> Right <$> parseJSON str <|> Left <$> parseJSON str)
      desc <- a .:? "error_description"
      errorUri <- a .:? "error_uri"
      return $ OAuth2Error err desc errorUri
  parseJSON _ = fail "Expected an object"

instance ToJSON err => ToJSON (OAuth2Error err) where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}

parseOAuth2Error :: FromJSON err => BSL.ByteString -> OAuth2Error err
parseOAuth2Error string =
  either (mkDecodeOAuth2Error string) id (eitherDecode string)

mkDecodeOAuth2Error :: BSL.ByteString -> String -> OAuth2Error err
mkDecodeOAuth2Error response err =
  OAuth2Error
    (Left "Decode error")
    (Just $ pack $ "Error: " <> err <> "\n Original Response:\n" <> show (decodeUtf8 $ BSL.toStrict response))
    Nothing

{-
TODO: looks like we always put credential to header. Do we still need 'ClientSecretBasic'?
Double check the spec whether it has to choose one way or another?
Given the testing experience, seems not.
-}

-- | https://www.rfc-editor.org/rfc/rfc6749#section-2.3
data ClientAuthenticationMethod
  = ClientSecretBasic
  | ClientSecretPost
  deriving (Eq, Ord)

--------------------------------------------------

-- * Types Synonym

--------------------------------------------------

-- | type synonym of post body content
type PostBody = [(BS.ByteString, BS.ByteString)]

type QueryParams = [(BS.ByteString, BS.ByteString)]

--------------------------------------------------

-- * Utilies

--------------------------------------------------

defaultRequestHeaders :: [(HT.HeaderName, BS.ByteString)]
defaultRequestHeaders =
  [ (HT.hUserAgent, "hoauth2"),
    (HT.hAccept, "application/json")
  ]

appendQueryParams :: [(BS.ByteString, BS.ByteString)] -> URIRef a -> URIRef a
appendQueryParams params =
  over (queryL . queryPairsL) (params ++)

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
            { secure = ssl,
              path = view pathL auri
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
