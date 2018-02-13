{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module IDP.StackExchange where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

data StackExchangeUser = StackExchangeUser { userId      :: Integer
                                           , displayName :: Text
                                           } deriving (Show, Generic)

instance FromJSON StackExchangeUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.stackexchange.com/2.2/me?site=stackoverflow|]

toLoginUser :: StackExchangeUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = displayName ouser }
