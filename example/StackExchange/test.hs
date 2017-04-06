{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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

import           Keys
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


main :: IO ()
main = do
    BS.putStrLn $ serializeURIRef' $ authorizationUrl stackexchangeKey
    putStrLn "visit the url and paste code here: "
    code <- fmap (ExchangeToken . T.pack) getLine
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr stackexchangeKey code
    print token
    case token of
      Right at -> siteInfo mgr (accessToken at) >>= print
      Left _   -> putStrLn "no access token found yet"

-- | Test API: info
siteInfo :: Manager -> AccessToken -> IO (OAuth2Result SiteInfo)
siteInfo mgr token = authGetJSON mgr token [uri|https://api.stackexchange.com/2.2/info?site=stackoverflow|]

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
