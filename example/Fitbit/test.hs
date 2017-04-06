{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Monad            (mzero)
import           Data.Aeson
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (chr)
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Network.HTTP.Conduit     hiding (Request, queryString)
import           Network.HTTP.Types       (Query, status200)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           URI.ByteString.QQ
import           URI.ByteString           (serializeURIRef')

import           Keys                     (fitbitKey)
import           Network.OAuth.OAuth2

------------------------------------------------------------------------------

data FitbitUser = FitbitUser
    { userId   :: Text
    , userName :: Text
    , userAge  :: Int
    } deriving (Show, Eq)

instance FromJSON FitbitUser where
    parseJSON (Object o) =
        FitbitUser
        <$> ((o .: "user") >>= (.: "encodedId"))
        <*> ((o .: "user") >>= (.: "fullName"))
        <*> ((o .: "user") >>= (.: "age"))
    parseJSON _ = mzero

instance ToJSON FitbitUser where
    toJSON (FitbitUser fid name age) =
        object [ "id"       .= fid
               , "name"     .= name
               , "age"      .= age
               ]

------------------------------------------------------------------------------

main :: IO ()
main = do
    print $ serializeURIRef' $ appendQueryParams [("state", state), ("scope", "profile")] $ authorizationUrl fitbitKey
    putStrLn "visit the url to continue"
    run 9988 application

state :: B.ByteString
state = "testFitbitApi"

application :: Application
application request respond = do
    response <- handleRequest requestPath request
    respond $ responseLBS status200 [("Content-Type", "text/plain")] response
  where
    requestPath = T.intercalate "/" $ pathInfo request

handleRequest :: Text -> Request -> IO BL.ByteString
handleRequest "favicon.ico" _ = return ""
handleRequest _ request = do
    mgr <- newManager tlsManagerSettings
    token <- getApiToken mgr $ getApiCode request
    print token
    user <- getApiUser mgr (accessToken token)
    print user
    return $ encode user

getApiCode :: Request -> ExchangeToken
getApiCode request =
    case M.lookup "code" queryMap of
        Just code -> ExchangeToken $ T.decodeUtf8 $ code
        Nothing -> error "request doesn't include code"
  where
    queryMap = convertQueryToMap $ queryString request

getApiToken :: Manager -> ExchangeToken -> IO (OAuth2Token)
getApiToken mgr code = do
    result <- doJSONPostRequest mgr fitbitKey url $ body ++ [("state", state)]
    case result of
        Right token -> return token
        Left e -> error $ lazyBSToString e
  where
    (url, body) = accessTokenUrl fitbitKey code

getApiUser :: Manager -> AccessToken -> IO (FitbitUser)
getApiUser mgr token = do
    result <- authGetJSON mgr token [uri|https://api.fitbit.com/1/user/-/profile.json|]
    case result of
        Right user -> return user
        Left e -> error $ lazyBSToString e

convertQueryToMap :: Query -> M.Map B.ByteString B.ByteString
convertQueryToMap query =
    M.fromList $ map normalize query
  where
    normalize (k, Just v) = (k, v)
    normalize (k, Nothing) = (k, B.empty)

lazyBSToString :: BL.ByteString -> String
lazyBSToString s = map (chr . fromIntegral) (BL.unpack s)
