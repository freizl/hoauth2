{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module IDP.Weibo where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import qualified Data.Text.Lazy    as TL
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

-- TODO: http://open.weibo.com/wiki/2/users/show
data WeiboUser = WeiboUser { id         :: Integer
                           , name       :: Text
                           , screenName :: Text
                           } deriving (Show, Generic)

newtype WeiboUID = WeiboUID { uid :: Integer }
  deriving (Show, Generic)

instance FromJSON WeiboUID where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON WeiboUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.weibo.com/2/account/get_uid.json|]

toLoginUser :: WeiboUID -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = TL.pack $ show $ uid ouser }
