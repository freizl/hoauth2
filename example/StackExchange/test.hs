{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Github API: http://developer.github.com/v3/oauth/

module Main where

import           Data.Aeson.TH         (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Network.HTTP.Conduit
import           URI.ByteString.QQ
import           URI.ByteString

import           Network.OAuth.OAuth2

import           Keys

data SiteInfo = SiteInfo { items           :: [SiteItem]
                         , has_more        :: Bool
                         , quota_max       :: Integer
                         , quota_remaining :: Integer
                         } deriving (Show, Eq)

data SiteItem = SiteItem { new_active_users       :: Integer
                           , total_users          :: Integer
                           , badges_per_minute    :: Double
                           , total_badges         :: Integer
                           , total_votes          :: Integer
                           , total_comments       :: Integer
                           , answers_per_minute   :: Double
                           , questions_per_minute :: Double
                           , total_answers        :: Integer
                           , total_accepted       :: Integer
                           , total_unanswered     :: Integer
                           , total_questions      :: Integer
                           , api_revision         :: Text
                         } deriving (Show, Eq)

$(deriveJSON defaultOptions ''SiteInfo)
$(deriveJSON defaultOptions ''SiteItem)


main :: IO ()
main = do
    BS.putStrLn $ serializeURIRef' $ authorizationUrl stackexchangeKey
    putStrLn "visit the url and paste code here: "
    code <- fmap (ExchangeToken . T.pack) getLine
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr stackexchangeKey code
    print token
    case token of
      Right at  -> siteInfo mgr (accessToken at) >>= print
      Left _    -> putStrLn "no access token found yet"

-- | Test API: info
siteInfo :: Manager -> AccessToken -> IO (OAuth2Result SiteInfo)
siteInfo mgr token = authGetJSON mgr token [uri|https://api.stackexchange.com/2.2/info?site=stackoverflow|]

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
