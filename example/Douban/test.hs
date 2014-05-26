{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-

douban oauth2: http://developers.douban.com/wiki/?title=oauth2

-}

module Main where

import           Control.Monad.Trans.Control     (MonadBaseControl)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Conduit                    (MonadResource)
import           Data.Maybe                      (fromJust)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types              as HT

import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import           Keys

main :: IO ()
main = do
          print $ authorizationUrl doubanKey
          putStrLn "visit the url and paste code here: "
          code <- getLine
          token <- fetchAccessToken doubanKey (sToBS code)
          print token
          case token of
            Right r -> do
                       uid <- authGetBS r "https://api.douban.com/v2/user/~me"
                       print uid
            Left l -> BSL.putStrLn l

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack
