{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  A simple OAuth2 client.
-}

module Network.OAuth2.HTTP.HttpClient 
       ( OAuth2 (..)
       , AccessToken (..)
       , authorizationUrl
       , postAccessToken
       , request
       , signRequest
       )
       where

import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit
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

data OAuthException = OAuthException String
                      deriving (Show, Eq, Typeable)

instance Exception OAuthException

-- | The gained Access Token 
data AccessToken = AccessToken { accessToken :: BS.ByteString } deriving (Show)

instance FromJSON AccessToken where
    parseJSON (Object o) = AccessToken <$> o .: "access_token"
    parseJSON _ = mzero

-- | Prepare the authorization URL
authorizationUrl :: OAuth2 -> BS.ByteString
authorizationUrl oa = oauthOAuthorizeEndpoint oa `BS.append` queryStr
  where queryStr = renderSimpleQuery True query
        query = transformParam [ ("client_id", Just $ oauthClientId oa)
                               , ("response_type", Just "code")
                               , ("redirect_uri", oauthCallback oa)]

request  :: Control.Monad.Trans.Resource.ResourceIO m =>
            Request m -> m (Response BSL.ByteString)
request req = (withManager . httpLbs) (req { checkStatus = \_ _ -> Nothing })

postAccessToken' :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO BSL.ByteString
postAccessToken' oa code grant_type = do
    rsp <- request req
    if (HT.statusCode . statusCode) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)
  where
    req = urlEncodedBody query . fromJust $ parseUrl url
    url = BS.unpack $ oauthAccessTokenEndpoint oa
    query = transformParam [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("code", Just code)
                          , ("redirect_uri", oauthCallback oa)
                          , ("grant_type", grant_type) ]

-- | lift value in the Maybe and abonda Nothing
transformParam :: [(a, Maybe b)] -> [(a, b)]
transformParam = foldr step' []
                 where step' :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
                       step' (a, Just b) xs = (a, b):xs
                       step' _ xs = xs

-- | Request (POST method) access token URL in order to get @AccessToken@.
postAccessToken :: OAuth2 
                -> BS.ByteString          -- ^ Authentication code gained after authorization
                -> IO (Maybe AccessToken)
postAccessToken oa code = decode <$> postAccessToken' oa code (Just "authorization_code")

-- | insert access token into the request
signRequest :: OAuth2 -> Request m -> Request m
signRequest oa req = req { queryString = renderSimpleQuery False newQuery }
  where
    newQuery = case oauthAccessToken oa of
                    Just at -> insert ("access_token", at) oldQuery   -- ^ TODO: allow access_token to be configuable
                    _ -> insert ("client_id", oauthClientId oa) . insert ("client_secret", oauthClientSecret oa) $ oldQuery
    oldQuery = parseSimpleQuery (queryString req)
