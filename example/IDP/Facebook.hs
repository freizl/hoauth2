{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}

module IDP.Facebook where
import           Data.Aeson
import           Data.Aeson.Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Text.Lazy                       (Text)
import           GHC.Generics
import Types

data FacebookUser = FacebookUser { id :: Text
                                 , name :: Text
                                 , email :: Text
                                 } deriving (Show, Generic)

instance FromJSON FacebookUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://graph.facebook.com/me?fields=id,name,email|]

toLoginUser :: FacebookUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }
