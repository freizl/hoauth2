{-# LANGUAGE OverloadedStrings #-}


module Api where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Aeson
--import qualified Data.ByteString as BS
import Data.Typeable (Typeable)
import Network.HTTP.Types (renderSimpleQuery)
import Control.Exception
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.HTTP.Types as HT


import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2

import Utils

accountUidUri :: BS.ByteString
accountUidUri = pack' "https://api.weibo.com/2/account/get_uid.json"

accountShowUri :: BS.ByteString
accountShowUri = pack' "https://api.weibo.com/2/users/show.json"

pack' = T.encodeUtf8 . T.pack


-- | UID
data WeiboUserId = WeiboUserId { weiboUserId :: Int } deriving (Show)

instance FromJSON WeiboUserId where
    parseJSON (Object o) = WeiboUserId <$> o .: "uid"
    parseJSON _ = mzero

apiUrlGet2 :: URI          -- ^ Base URI
          -> (AccessToken, WeiboUserId)  -- ^ Authorized Access Token and UID
          -> URI          -- ^ Combined Result
apiUrlGet2 uri (token, uid) = uri `BS.append` (renderSimpleQuery True $ 
                                               (accessTokenToParam token ++ uidToParam uid))


uidToParam :: WeiboUserId -> [(BS.ByteString, BS.ByteString)]
uidToParam (WeiboUserId uid) = [("uid", intToByteString uid)]


-- | Fetch UID
-- 
requestUid :: URI              -- ^ Fetch UID API URI
           -> AccessToken
           -> IO (Maybe WeiboUserId)
requestUid uri token = decode <$> requestUid' uri token

requestUid' :: URI 
           -> AccessToken
           -> IO BSL.ByteString
requestUid' uri token = doSimpleGetRequest (BS8.unpack $ apiUrlGet uri token) >>= retOrError
  where
    retOrError rsp = if (HT.statusCode . responseStatus) rsp == 200
                        --then (print $ responseBody rsp) >> (return $ responseBody rsp)
                        then return $ responseBody rsp
                        else throwIO . OAuthException $ "Gaining uid failed: " ++ BSL.unpack (responseBody rsp)