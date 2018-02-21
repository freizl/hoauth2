{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings         #-}

{-
  NOTES: stackexchange API spec and its document just sucks!
-}
module IDP.StackExchange where
import           Data.Aeson
import           Data.ByteString                   (ByteString)
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import qualified Data.Text.Lazy    as TL
import           GHC.Generics
import           Types
import           Lens.Micro
import           URI.ByteString
import           URI.ByteString.QQ
import           Network.HTTP.Conduit
import           Data.Bifunctor
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import TokenUtil
import Keys

data StackExchangeResp = StackExchangeResp { hasMore :: Bool
                                           , quotaMax :: Integer
                                           , quotaRemaining :: Integer
                                           , items :: [StackExchangeUser]
                                           } deriving (Show, Generic)

data StackExchangeUser = StackExchangeUser { userId       :: Integer
                                           , displayName  :: Text
                                           , profileImage :: Text
                                           } deriving (Show, Generic)

instance FromJSON StackExchangeResp where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON StackExchangeUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.stackexchange.com/2.2/me?site=stackoverflow|]

toLoginUser :: StackExchangeResp -> LoginUser
toLoginUser StackExchangeResp {..} =
  case items of
    [] -> LoginUser { loginUserName = TL.pack "Cannot find stackexchange user" }
    (user:_) -> LoginUser { loginUserName = displayName user }

getUserInfo :: FromJSON a => Manager -> AccessToken -> IO (OAuth2Result a LoginUser)
getUserInfo mgr at = do
  re <- parseResponseJSON
        <$> authGetBS2 mgr at
            (userInfoUri `appendStackExchangeAppKey` stackexchangeAppKey)
  return (second toLoginUser re)

appendStackExchangeAppKey :: URI -> ByteString -> URI
appendStackExchangeAppKey uri k =
  over (queryL . queryPairsL) (\query -> query ++ [("key", k)]) uri

getAccessToken :: Manager
               -> OAuth2
               -> ExchangeToken
               -> IO (OAuth2Result TR.Errors OAuth2Token)
getAccessToken = postAT
