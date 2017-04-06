{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK -ignore-exports #-}

-- | A simple OAuth2 Haskell binding.  (This is supposed to be
-- independent of the http client used.)

module Network.OAuth.OAuth2.Internal where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe
import           Data.Text.Encoding
import           Data.Text              (Text)
import           GHC.Generics
import           URI.ByteString
import           Lens.Micro
import           Lens.Micro.Extras
import           Network.HTTP.Conduit as C
import           Control.Monad.Catch
import qualified Network.HTTP.Types           as H

--------------------------------------------------
-- * Data Types
--------------------------------------------------

-- | Query Parameter Representation
data OAuth2 = OAuth2 {
      oauthClientId            :: Text
    , oauthClientSecret        :: Text
    , oauthOAuthorizeEndpoint  :: URI
    , oauthAccessTokenEndpoint :: URI
    , oauthCallback            :: Maybe (URI)
    } deriving (Show, Eq)

newtype AccessToken = AccessToken { atoken :: Text } deriving (Show, FromJSON, ToJSON)
newtype RefreshToken = RefreshToken { rtoken :: Text } deriving (Show, FromJSON, ToJSON)
newtype IdToken = IdToken { idtoken :: Text } deriving (Show, FromJSON, ToJSON)
newtype ExchangeToken = ExchangeToken { extoken :: Text } deriving (Show, FromJSON, ToJSON)


-- | The gained Access Token. Use @Data.Aeson.decode@ to
-- decode string to @AccessToken@.  The @refreshToken@ is
-- special in some cases,
-- e.g. <https://developers.google.com/accounts/docs/OAuth2>
data OAuth2Token = OAuth2Token {
      accessToken  :: AccessToken
    , refreshToken :: Maybe RefreshToken
    , expiresIn    :: Maybe Int
    , tokenType    :: Maybe Text
    , idToken      :: Maybe IdToken
    } deriving (Show, Generic)

-- | Parse JSON data into 'OAuth2Token'
instance FromJSON OAuth2Token where
    parseJSON = (genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' })
instance ToJSON OAuth2Token where
    toEncoding = (genericToEncoding defaultOptions { fieldLabelModifier = camelTo2 '_' })

--------------------------------------------------
-- * Types Synonym
--------------------------------------------------

-- | Is either 'Left' containing an error or 'Right' containg a result
type OAuth2Result a = Either BSL.ByteString a

-- | type synonym of post body content
type PostBody = [(BS.ByteString, BS.ByteString)]

type QueryParams = [(BS.ByteString, BS.ByteString)]

--------------------------------------------------
-- * URLs
--------------------------------------------------

-- | Prepare the authorization URL.  Redirect to this URL
-- asking for user interactive authentication.
authorizationUrl :: OAuth2 -> URI
authorizationUrl oa = over (queryL . queryPairsL) (\l -> l ++ queryParts) (oauthOAuthorizeEndpoint oa)
  where queryParts = catMaybes [ Just ("client_id", encodeUtf8 $ oauthClientId oa)
                               , Just ("response_type", "code")
                               , fmap ("redirect_uri",) (fmap serializeURIRef' $ oauthCallback oa) ]

-- | Prepare the URL and the request body query for fetching an access token.
accessTokenUrl :: OAuth2
                  -> ExchangeToken    -- ^ access code gained via authorization URL
                  -> (URI, PostBody)  -- ^ access token request URL plus the request body.
accessTokenUrl oa code = accessTokenUrl' oa code (Just "authorization_code")

-- | Prepare the URL and the request body query for fetching an access token, with
-- optional grant type.
accessTokenUrl' ::  OAuth2
                    -> ExchangeToken   -- ^ access code gained via authorization URL
                    -> Maybe Text      -- ^ Grant Type
                    -> (URI, PostBody) -- ^ access token request URL plus the request body.
accessTokenUrl' oa code gt = (uri, body)
  where uri  = oauthAccessTokenEndpoint oa
        body = catMaybes [ Just ("code", encodeUtf8 $ extoken code)
                         , fmap (("redirect_uri",) . serializeURIRef') $ oauthCallback oa
                         , fmap (("grant_type",) . encodeUtf8) gt
                         ]

-- | Using a Refresh Token.  Obtain a new access token by
-- sending a refresh token to the Authorization server.
refreshAccessTokenUrl :: OAuth2
                         -> RefreshToken     -- ^ refresh token gained via authorization URL
                         -> (URI, PostBody)  -- ^ refresh token request URL plus the request body.
refreshAccessTokenUrl oa token = (uri, body)
  where uri = oauthAccessTokenEndpoint oa
        body = [ ("grant_type", "refresh_token")
               , ("refresh_token", encodeUtf8 $ rtoken token)
               ]

-- | For `GET` method API.
appendAccessToken :: URIRef a             -- ^ Base URI
                     -> AccessToken       -- ^ Authorized Access Token
                     -> URIRef a          -- ^ Combined Result
appendAccessToken uri t = over (queryL . queryPairsL) (\query -> query ++ (accessTokenToParam t)) uri

-- | Create 'QueryParams' with given access token value.
accessTokenToParam :: AccessToken -> [(BS.ByteString, BS.ByteString)]
accessTokenToParam t = [("access_token", encodeUtf8 $ atoken t)]

appendQueryParams :: [(BS.ByteString, BS.ByteString)] -> URIRef a -> URIRef a
appendQueryParams params =
  over (queryL . queryPairsL) (params ++ )

uriToRequest :: MonadThrow m => URI -> m Request
uriToRequest uri = do
  ssl <- case (view (uriSchemeL . schemeBSL) uri) of
    "http" -> return False
    "https" -> return True
    s -> throwM $ InvalidUrlException (show uri) ("Invalid scheme: " ++ show s)
  let
    query = fmap (\(a, b) -> (a, Just b)) (view (queryL . queryPairsL) uri)
    hostL = (authorityL . _Just . authorityHostL . hostBSL)
    portL = (authorityL . _Just . authorityPortL . _Just . portNumberL)
    defaultPort = (if ssl then 443 else 80) :: Int

    req = (setQueryString query) $ defaultRequest {
        secure = ssl,
        path = (view pathL uri)
      }
    req2 = (over hostLens . maybe id const . preview hostL) uri req
    req3 = (over portLens . maybe (\_ -> defaultPort) const . preview portL) uri req2
  return $ req3

requestToUri :: Request -> URI
requestToUri req =
  URI
    (Scheme (if secure req
           then "https"
           else "http"))
    (Just (Authority Nothing (Host $ host req) (Just $ Port $ port req)))
    (path req)
    (Query $ H.parseSimpleQuery $ queryString req)
    Nothing

hostLens :: Lens' Request BS.ByteString
hostLens f req = f (C.host req) <&> \h' -> req { C.host = h' }
{-# INLINE hostLens #-}

portLens :: Lens' Request Int
portLens f req = f (C.port req) <&> \p' -> req { C.port = p' }
{-# INLINE portLens #-}
