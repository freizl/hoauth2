{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

module IDP.Weibo where
import           Data.Aeson
import           Data.Aeson.Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Text.Lazy                       (Text)
import           GHC.Generics
import Types

data WeiboUser = WeiboUser { name :: Text
                         , preferredUsername :: Text
                         } deriving (Show, Generic)

instance FromJSON WeiboUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://dev-148986.oktapreview.com/oauth2/v1/userinfo|]

toLoginUser :: WeiboUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }
