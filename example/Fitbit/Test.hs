{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fitbit.Test where

import           Control.Applicative
import           Control.Monad            (mzero)
import           Data.Aeson
import           Data.Char                (chr)
import           Data.Text                (Text)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import           Network.HTTP.Conduit     hiding (Request,queryString,port)
import           Network.Wai              (Request,queryString)
import           Network.HTTP.Types       (Query)

import           Network.OAuth.OAuth2
import           Keys                     (fitbitKey)

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

state :: B.ByteString
state = "testFitbitApi"

handleFitbitRequest :: Request -> IO BL.ByteString
handleFitbitRequest request = do
    mgr <- newManager conduitManagerSettings
    token <- getApiToken mgr $ getApiCode request
    print token
    user <- getApiUser mgr token
    print user
    closeManager mgr
    return $ encode user

getApiCode :: Request -> B.ByteString
getApiCode request =
    case M.lookup "code" queryMap of
        Just code -> code
        Nothing -> error "request doesn't include code"
  where
    queryMap = convertQueryToMap $ queryString request

getApiToken :: Manager -> B.ByteString -> IO (AccessToken)
getApiToken mgr code = do
    result <- doJSONPostRequest mgr fitbitKey url $ body ++ [("state", state)]
    case result of
        Right token -> return token
        Left e -> error $ lazyBSToString e
  where
    (url, body) = accessTokenUrl fitbitKey code

getApiUser :: Manager -> AccessToken -> IO (FitbitUser)
getApiUser mgr token = do
    result <- authGetJSON mgr token "https://api.fitbit.com/1/user/-/profile.json"
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
