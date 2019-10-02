{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

{-
  NOTES: stackexchange API spec and its document just sucks!
-}
module IDP.StackExchange where
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString      (ByteString)
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
import qualified Data.ByteString.Lazy.Char8        as BSL
import           GHC.Generics
import           Keys
import           Lens.Micro
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

data StackExchange = StackExchange deriving (Show, Generic)

instance Hashable StackExchange

instance IDP StackExchange

instance HasLabel StackExchange

instance HasTokenReq StackExchange where
  tokenReq _ mgr = fetchAccessToken2 mgr stackexchangeKey

instance HasUserReq StackExchange where
  userReq _ mgr token = do
    re <- authGetBS2 mgr token
              (userInfoUri `appendStackExchangeAppKey` stackexchangeAppKey)
    return (re >>= (bimap BSL.pack toLoginUser . eitherDecode))

instance HasAuthUri StackExchange where
  authUri _ = createCodeUri stackexchangeKey [ ("state", "StackExchange.test-state-123")
                                          ]

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

appendStackExchangeAppKey :: URI -> ByteString -> URI
appendStackExchangeAppKey useruri k =
  over (queryL . queryPairsL) (\query -> query ++ [("key", k)]) useruri
