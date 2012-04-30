{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  A simple OAuth2 Haskell binding. 
  This is sopposed to be independent with http client.
-}

module Network.OAuth2.OAuth2
       ( OAuth2 (..)
       , AccessToken (..)
       , OAuthException (..)
       , authorizationUrl
       , accessTokenUrl
       ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Typeable (Typeable)
import Network.HTTP.Types (renderSimpleQuery)
import Control.Exception
import Control.Applicative ((<$>))
import Control.Monad (mzero)

-- | Query Parameter Representation
data OAuth2 = OAuth2 { oauthClientId :: BS.ByteString
                     , oauthClientSecret :: BS.ByteString
                     , oauthOAuthorizeEndpoint :: BS.ByteString
                     , oauthAccessTokenEndpoint :: BS.ByteString
                     , oauthCallback :: Maybe BS.ByteString
                     , oauthAccessToken :: Maybe BS.ByteString
                     } deriving (Show, Eq)

-- | Simple Exception representation.
data OAuthException = OAuthException String
                      deriving (Show, Eq, Typeable)

instance Exception OAuthException

-- | The gained Access Token. Use @Data.Aeson.decode@ to decode string to @AccessToken@.
data AccessToken = AccessToken { accessToken :: BS.ByteString } deriving (Show)

instance FromJSON AccessToken where
    parseJSON (Object o) = AccessToken <$> o .: "access_token"
    parseJSON _ = mzero

--------------------------------------------------
-- Parameter Util    
    
-- | type synonym of query parameters    
type QueryParams = [(BS.ByteString, BS.ByteString)]

-- | type synonym of post body content
type PostBody = [(BS.ByteString, BS.ByteString)]

-- | type synonym of a URI
type URI = BS.ByteString

-- | Append query parameters
appendQueryParam :: URI -> QueryParams -> URI
appendQueryParam uri q = uri `BS.append` renderSimpleQuery True q

-- | lift value in the Maybe and abonda Nothing
transform' :: [(a, Maybe b)] -> [(a, b)]
transform' = foldr step' []
                 where step' :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
                       step' (a, Just b) xs = (a, b):xs
                       step' _ xs = xs

--------------------------------------------------
-- oauth request urls

-- | Prepare the authorization URL. Redirect to this URL asking for user interactive authentication.
authorizationUrl :: OAuth2 -> URI
authorizationUrl oa = (oauthOAuthorizeEndpoint oa) `appendQueryParam` queryStr
  where queryStr = transform' [ ("client_id", Just $ oauthClientId oa)
                              , ("response_type", Just "code")
                              , ("redirect_uri", oauthCallback oa)]


-- | Prepare access token URL and the request body query.

accessTokenUrl :: OAuth2 
               -> BS.ByteString    -- ^ access code gained via authorization URL
               -> (URI, PostBody)  -- ^ access token request URL plus the request body.
accessTokenUrl oa code = accessTokenUrl' oa code (Just "authorization_code")               


accessTokenUrl' ::  OAuth2 
                 -> BS.ByteString         -- ^ access code gained via authorization URL
                 -> Maybe BS.ByteString   -- ^ Grant Type
                 -> (URI, PostBody)       -- ^ access token request URL plus the request body.
accessTokenUrl' oa code gt = (uri, body)           
  where uri  = oauthAccessTokenEndpoint oa
        body = transform' [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("code", Just code)
                          , ("redirect_uri", oauthCallback oa)
                          , ("grant_type", gt) ]

