{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Github API: http://developer.github.com/v3/oauth/

module Main where

import           Control.Applicative
import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Network.HTTP.Conduit

import           Network.OAuth.OAuth2

import           Keys

data SiteInfo = SiteInfo { items   :: [SiteItem]
                         , has_more :: Bool
                         , quota_max :: Integer
                         , quota_remaining :: Integer
                         } deriving (Show, Eq)

data SiteItem = SiteItem { new_active_users :: Integer
                           , total_users :: Integer
                           , badges_per_minute :: Double
                           , total_badges :: Integer
                           , total_votes :: Integer
                           , total_comments :: Integer
                           , answers_per_minute :: Double
                           , questions_per_minute :: Double
                           , total_answers :: Integer
                           , total_accepted :: Integer
                           , total_unanswered :: Integer
                           , total_questions :: Integer
                           , api_revision :: Text
                         } deriving (Show, Eq)

$(deriveJSON defaultOptions ''SiteInfo)
$(deriveJSON defaultOptions ''SiteItem)


main :: IO ()
main = do
    print $ authorizationUrl stackexchangeKey
    putStrLn "visit the url and paste code here: "
    code <- getLine
    mgr <- newManager tlsManagerSettings
    let (url, body) = accessTokenUrl stackexchangeKey (sToBS code)
    token <- doSimplePostRequest mgr stackexchangeKey url (body)
    print (token :: OAuth2Result BSL.ByteString)
    case token of
      Right at  -> siteInfo mgr (getAccessToken at) >>= print
      Left _    -> putStrLn "no access token found yet"



-- stackexchange access token api does not respond json but an string
-- https://api.stackexchange.com/docs/authentication
getAccessToken :: BSL.ByteString -> AccessToken
getAccessToken str = let xs = BSL.split '&' str
                         ys = BSL.split '=' (xs !! 0)
                     in
                       AccessToken { accessToken = BSL.toStrict (ys !! 1)
                                   , refreshToken = Nothing
                                   , expiresIn = Nothing
                                   , tokenType = Nothing
                                   }

-- | Test API: info
siteInfo :: Manager -> AccessToken -> IO (OAuth2Result SiteInfo)
siteInfo mgr token = authGetJSON mgr token "https://api.stackexchange.com/2.2/info?site=stackoverflow"

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
