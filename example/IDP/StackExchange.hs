{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE RecordWildCards           #-}

{-
  NOTES: stackexchange API spec and its document just sucks!
-}
module IDP.StackExchange where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import qualified Data.Text.Lazy as TL
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

data StackExchangeResp = StackExchangeResp { hasMore :: Bool
                                           , quotaMax :: Integer
                                           , quotaRemaining :: Integer
                                           , items :: [StackExchangeUser]
                                           } deriving (Show, Generic)

data StackExchangeUser = StackExchangeUser { userId      :: Integer
                                           , displayName :: Text
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
    (user:_) -> LoginUser { loginUserName = (displayName user) }
