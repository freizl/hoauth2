{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Fitbit.Test where

import           Control.Applicative
import           Control.Monad            (mzero)
import           Data.Aeson
import           Data.Text                (Text)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Network.HTTP.Conduit     hiding (Request,queryString,port)
import           Network.Wai              (Request)

import           Network.OAuth.OAuth2
import           Keys                     (fitbitKey)
import           Common

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
