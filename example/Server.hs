{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Text                (Text)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Network.Wai
import           Network.HTTP.Types       (status200)
import           Network.Wai.Handler.Warp (run)

import           Network.OAuth.OAuth2
import           Keys                     (fitbitKey)
import           Fitbit.Test              (handleFitbitRequest)

------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "Visit http://localhost:" ++ show port
    run port application
  where
    port = 9988

fitbitAuthorizationUrl :: B.ByteString
fitbitAuthorizationUrl = authorizationUrl fitbitKey `appendQueryParam` [("state", state), ("scope", "profile")]

state :: B.ByteString
state = "testHoauth2"

application :: Application
application request respond = do
    response <- handleRequest requestPath request
    respond $ responseLBS status200 [] response
  where
    requestPath = pathInfo request

handleRequest :: [Text] -> Request -> IO BL.ByteString
handleRequest ([]) _ = return $ BL.concat
    ["<a href='", BL.fromStrict fitbitAuthorizationUrl, "'>Test Fitbit API</a>"]
handleRequest ("fitbit":_) request = handleFitbitRequest request
handleRequest ("favicon.ico":[]) _ = return ""
handleRequest requestPath _ = error $ "wrong url" ++ show requestPath
