{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

{-
  NOTES: stackexchange API spec and its document just sucks!
-}
module IDP.StackExchange where

import Control.Monad.Trans.Except
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Hashable
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           GHC.Generics
import           Lens.Micro
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

userInfoUri :: URI
userInfoUri = [uri|https://api.stackexchange.com/2.2/me?site=stackoverflow|]

type AppKey = ByteString

data StackExchange = StackExchange OAuth2 AppKey deriving (Show, Generic, Eq)

instance Hashable StackExchange

instance IDP StackExchange

instance HasLabel StackExchange where
  idpLabel = const "StackExchange"

instance HasTokenReq StackExchange where
  tokenReq (StackExchange key _) mgr = fetchAccessTokenClientCredInBoth mgr key

instance HasTokenRefreshReq StackExchange where
  tokenRefreshReq (StackExchange key _) mgr = refreshAccessTokenClientCredInBoth mgr key

instance HasUserReq StackExchange where
  userReq (StackExchange _ appKey) mgr token = do
    re <- authGetBS [AuthInRequestQuery] mgr token
              (userInfoUri `appendStackExchangeAppKey` appKey)
    case eitherDecode re of
      Right obj -> return (toLoginUser obj)
      Left e -> throwE (BSL.pack e)

instance HasAuthUri StackExchange where
  authUri (StackExchange key _) = createCodeUri key [("state", "StackExchange.test-state-123")]

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

toLoginUser :: StackExchangeResp -> LoginUser
toLoginUser StackExchangeResp {..} =
  case items of
    [] -> LoginUser { loginUserName = TL.pack "Cannot find stackexchange user" }
    (user:_) -> LoginUser { loginUserName = displayName user }

appendStackExchangeAppKey :: URI -> ByteString -> URI
appendStackExchangeAppKey useruri k =
  over (queryL . queryPairsL) (\query -> query ++ [("key", k)]) useruri
