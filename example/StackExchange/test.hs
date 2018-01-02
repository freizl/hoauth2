{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | https://api.stackexchange.com/docs/authentication

module Main where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           GHC.Generics
import           Network.HTTP.Conduit
import           URI.ByteString
import           URI.ByteString.QQ

import           Keys                  (stackexchangeKey)
import           Network.OAuth.OAuth2

data SiteInfo = SiteInfo { items          :: [SiteItem]
                         , hasMore        :: Bool
                         , quotaMax       :: Integer
                         , quotaRemaining :: Integer
                         } deriving (Show, Eq, Generic)

data SiteItem = SiteItem { newActiveUsers       :: Integer
                           , totalUsers         :: Integer
                           , badgesPerMinute    :: Double
                           , totalBadges        :: Integer
                           , totalVotes         :: Integer
                           , totalComments      :: Integer
                           , answersPerMinute   :: Double
                           , questionsPerMinute :: Double
                           , totalAnswers       :: Integer
                           , totalAccepted      :: Integer
                           , totalUnanswered    :: Integer
                           , totalQuestions     :: Integer
                           , apiRevision        :: Text
                         } deriving (Show, Eq, Generic)

instance FromJSON SiteInfo where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON SiteInfo where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON SiteItem where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON SiteItem where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = camelTo2 '_' }

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }



main :: IO ()
main = do
    BS.putStrLn $ serializeURIRef' $ authorizationUrl stackexchangeKey
    putStrLn "visit the url and paste code here: "
    code <- fmap (ExchangeToken . T.pack) getLine
    mgr <- newManager tlsManagerSettings
    let (url, body) = accessTokenUrl stackexchangeKey code
    let extraBody = [ ("state", "test")
                    , ("client_id", T.encodeUtf8 $ oauthClientId stackexchangeKey)
                    , ("client_secret", T.encodeUtf8 $ oauthClientSecret stackexchangeKey)
                    ]

    -- NOTE: stackexchange doesn't really comply with standard, its access token response looks like
    -- `access_token=...&expires=1234`.
    -- the `doFlexiblePostRequest` is able to convert it to OAuth2Token type
    -- but the `expires` is lost given standard naming is `expires_in`
    token <- doFlexiblePostRequest mgr stackexchangeKey url (extraBody ++ body)
    print token
    case token of
      Right at -> siteInfo mgr (accessToken at) >>= print
      Left (_ :: OAuth2Error Errors) -> putStrLn "no access token found yet"

-- | Test API: info
siteInfo :: Manager -> AccessToken -> IO (OAuth2Result (OAuth2Error Errors) SiteInfo)
siteInfo mgr token = authGetJSON mgr token [uri|https://api.stackexchange.com/2.2/info?site=stackoverflow|]

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
