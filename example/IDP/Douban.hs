{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

module IDP.Douban where
import           Data.Aeson
import           Data.Aeson.Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Text.Lazy                       (Text)
import           GHC.Generics
import Types

data DoubanUser = DoubanUser { name :: Text
                             , uid :: Text
                             } deriving (Show, Generic)

instance FromJSON DoubanUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.douban.com/v2/user/~me|]

toLoginUser :: DoubanUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }
