{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_HADDOCK -ignore-exports #-}

-- | A simple OAuth2 Haskell binding.
--   (This is supposed to be independent with http client.)

module Network.OAuth.OAuth2.Internal where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Monad        (mzero)
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe
import           Data.Text.Encoding
import           Network.HTTP.Types   (renderSimpleQuery)

--------------------------------------------------
-- * Data Types
--------------------------------------------------

-- | Query Parameter Representation
--
data OAuth2 = OAuth2 {
      oauthClientId            :: BS.ByteString
    , oauthClientSecret        :: BS.ByteString
    , oauthOAuthorizeEndpoint  :: BS.ByteString
    , oauthAccessTokenEndpoint :: BS.ByteString
    , oauthCallback            :: Maybe BS.ByteString
    } deriving (Show, Eq)


-- | The gained Access Token. Use @Data.Aeson.decode@ to decode string to @AccessToken@.
--   The @refresheToken@ is special at some case.
--   e.g. https://developers.google.com/accounts/docs/OAuth2
--
data AccessToken = AccessToken {
      accessToken  :: BS.ByteString
    , refreshToken :: Maybe BS.ByteString
    } deriving (Show)

-- | Parse JSON data into {AccessToken}
--
instance FromJSON AccessToken where
    parseJSON (Object o) = AccessToken <$> at <*> rt where
        at = fmap encodeUtf8 $ o .: "access_token"
        rt = fmap (fmap encodeUtf8) $ o .:? "refresh_token"
    parseJSON _ = mzero

--------------------------------------------------
-- * Types Synonym
--------------------------------------------------

-- | Is either 'Left' containing an error or 'Right' containg a result
--
type OAuth2Result a = Either BSL.ByteString a

-- | type synonym of query parameters
type QueryParams = [(BS.ByteString, BS.ByteString)]

-- | type synonym of post body content
type PostBody = [(BS.ByteString, BS.ByteString)]

-- | type synonym of a URI
type URI = BS.ByteString


--------------------------------------------------
-- * URLs
--------------------------------------------------

-- | Prepare the authorization URL.
--   Redirect to this URL asking for user interactive authentication.
--
authorizationUrl :: OAuth2 -> URI
authorizationUrl oa = oauthOAuthorizeEndpoint oa `appendQueryParam` queryStr
  where queryStr = transform' [ ("client_id", Just $ oauthClientId oa)
                              , ("response_type", Just "code")
                              , ("redirect_uri", oauthCallback oa)]


-- | Prepare URL and the request body query for fetching access token.
--
accessTokenUrl :: OAuth2
                  -> BS.ByteString       -- ^ access code gained via authorization URL
                  -> (URI, PostBody)     -- ^ access token request URL plus the request body.
accessTokenUrl oa code = accessTokenUrl' oa code (Just "authorization_code")

accessTokenUrl' ::  OAuth2
                    -> BS.ByteString          -- ^ access code gained via authorization URL
                    -> Maybe BS.ByteString    -- ^ Grant Type
                    -> (URI, PostBody)        -- ^ access token request URL plus the request body.
accessTokenUrl' oa code gt = (uri, body)
  where uri  = oauthAccessTokenEndpoint oa
        body = transform' [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("code", Just code)
                          , ("redirect_uri", oauthCallback oa)
                          , ("grant_type", gt) ]

-- | Using a Refresh Token.
--   obtain a new access token by sending a refresh token to the Authorization server.
--
refreshAccessTokenUrl :: OAuth2
                         -> BS.ByteString    -- ^ refresh token gained via authorization URL
                         -> (URI, PostBody)  -- ^ refresh token request URL plus the request body.
refreshAccessTokenUrl oa rtoken = (uri, body)
  where uri = oauthAccessTokenEndpoint oa
        body = transform' [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("grant_type", Just "refresh_token")
                          , ("refresh_token", Just rtoken) ]

--------------------------------------------------
-- * UTILs
--------------------------------------------------

-- | Append query parameters with '?'
appendQueryParam :: URI -> QueryParams -> URI
appendQueryParam uri q = if "?" `BS.isInfixOf` uri
                         then uri `BS.append` "&" `BS.append` renderSimpleQuery False q
                         else uri `BS.append` renderSimpleQuery True q

-- | Append query parameters with '&'.
-- appendQueryParam' :: URI -> QueryParams -> URI
-- appendQueryParam' uri q = uri `BS.append` "&" `BS.append` renderSimpleQuery False q


-- | For GET method API.
appendAccessToken :: URI               -- ^ Base URI
                     -> AccessToken    -- ^ Authorized Access Token
                     -> URI            -- ^ Combined Result
appendAccessToken uri t = appendQueryParam uri (accessTokenToParam t)

-- | Create QueryParams with given access token value.
--
--accessTokenToParam :: BS.ByteString -> QueryParams
--accessTokenToParam token = [("access_token", token)]
accessTokenToParam :: AccessToken -> QueryParams
accessTokenToParam (AccessToken token _) = [("access_token", token)]


-- | lift value in the Maybe and abonda Nothing
transform' :: [(a, Maybe b)] -> [(a, b)]
transform' = map (\(a, Just b) -> (a, b)) . filter (isJust . snd)

