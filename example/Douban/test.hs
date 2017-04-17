{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-

douban oauth2: http://developers.douban.com/wiki/?title=oauth2

/v2/movie/nowplaying

-}

module Main where

import qualified Data.ByteString.Char8      as BS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.Encoding    as TL
import           Network.HTTP.Conduit
import           URI.ByteString
import           URI.ByteString.QQ

import           Network.OAuth.OAuth2

import           Keys                       (doubanKey)

main :: IO ()
main = do
  BS.putStrLn $ serializeURIRef' $ authorizationUrl doubanKey
  putStrLn "visit the url and paste code here: "
  code <- fmap (ExchangeToken . T.pack) getLine
  mgr <- newManager tlsManagerSettings
  let (url, body) = accessTokenUrl doubanKey code
  let extraBody = [ ("client_id", T.encodeUtf8 $ oauthClientId doubanKey)
                  , ("client_secret", T.encodeUtf8 $ oauthClientSecret doubanKey)
                  ]

  token <- doJSONPostRequest mgr doubanKey url (extraBody ++ body)
  print token
  case token of
    Right r -> do
      -- TODO: display Chinese character. (Text UTF-8 encodeing does not work, why?)
      uid <- authGetBS mgr (accessToken r) [uri|https://api.douban.com/v2/user/~me|]
      putStrLn $ either show (show . TL.decodeUtf8) uid
    Left l -> putStrLn $ show l

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
