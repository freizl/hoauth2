{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-

douban oauth2: http://developers.douban.com/wiki/?title=oauth2

/v2/movie/nowplaying

-}

module Main where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Conduit

import           Network.OAuth.OAuth2

import           Keys

main :: IO ()
main = do
  print $ authorizationUrl doubanKey
  putStrLn "visit the url and paste code here: "
  code <- getLine
  mgr <- newManager tlsManagerSettings
  token <- fetchAccessToken mgr doubanKey (sToBS code)
  print token
  case token of
    Right r -> do
      -- TODO: display Chinese character. (Text UTF-8 encodeing does not work, why?)
      uid <- authGetBS mgr r "https://api.douban.com/v2/user/~me"
      print uid
    Left l -> BSL.putStrLn l

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
