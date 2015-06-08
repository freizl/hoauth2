{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Instagram.Test where

import           Control.Applicative
import           Control.Monad            (mzero)
import           Data.Aeson
import           Data.Text                (Text)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Network.HTTP.Conduit     hiding (Request,queryString,port)
import           Network.Wai              (Request)

import           Network.OAuth.OAuth2
import           Keys                     (instagramKey)
import           Common

------------------------------------------------------------------------------

data InstagramUser = InstagramUser
    { userId   :: Text
    , userName :: Text
    , userFullName  :: Text
    } deriving (Show, Eq)

instance FromJSON InstagramUser where
    parseJSON (Object o) =
        InstagramUser
        <$> ((o .: "data") >>= (.: "id"))
        <*> ((o .: "data") >>= (.: "username"))
        <*> ((o .: "data") >>= (.: "full_name"))
    parseJSON _ = mzero

instance ToJSON InstagramUser where
    toJSON (InstagramUser iid username fullName) =
        object [ "id"        .= iid
               , "username"  .= username
               , "full_name" .= fullName
               ]

------------------------------------------------------------------------------

state :: B.ByteString
state = "testInstagramApi"

handleInstagramRequest :: Request -> IO BL.ByteString
handleInstagramRequest request = do
    mgr <- newManager conduitManagerSettings
    token <- getApiToken mgr $ getApiCode request
    print token
    user <- getApiUser mgr token
    print user
    closeManager mgr
    return $ encode user

getApiUser :: Manager -> AccessToken -> IO (InstagramUser)
getApiUser mgr token = do
    result <- authGetJSON mgr token $ B.concat ["https://api.instagram.com/v1/users/self?access_token=", accessToken token]
    case result of
        Right user -> return user
        Left e -> error $ lazyBSToString e

getApiToken :: Manager -> B.ByteString -> IO (AccessToken)
getApiToken mgr code = do
    result <- doJSONPostRequest mgr instagramKey url $ body ++ [("state", state)]
    case result of
        Right token -> return token
        Left e -> error $ lazyBSToString e
  where
    (url, body) = accessTokenUrl instagramKey code
