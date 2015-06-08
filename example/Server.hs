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
import           Keys                     (fitbitKey,instagramKey)
import           Fitbit.Test              (handleFitbitRequest)
import           Instagram.Test           (handleInstagramRequest)

------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn $ "Visit http://localhost:" ++ show port
    run port application
  where
    port = 9988

fitbitAuthorizationUrl :: B.ByteString
fitbitAuthorizationUrl = authorizationUrl fitbitKey `appendQueryParam` [("state", state), ("scope", "profile")]

instagramAuthorizationUrl :: B.ByteString
instagramAuthorizationUrl = authorizationUrl instagramKey `appendQueryParam` [("state", state)]

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
    [ "<a href='", BL.fromStrict fitbitAuthorizationUrl, "'>Test Fitbit API</a><br/>"
    , "<a href='", BL.fromStrict instagramAuthorizationUrl, "'>Test Instagram API</a><br/>"
    ]
handleRequest ("fitbit":_) request = handleFitbitRequest request
handleRequest ("instagram":_) request = handleInstagramRequest request
handleRequest ("favicon.ico":[]) _ = return ""
handleRequest requestPath _ = error $ "wrong url" ++ show requestPath
