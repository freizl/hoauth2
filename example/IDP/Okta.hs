{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module IDP.Okta where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Lazy    (Text)
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

data OktaUser = OktaUser { name              :: Text
                         , preferredUsername :: Text
                         } deriving (Show, Generic)

instance FromJSON OktaUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://dev-148986.oktapreview.com/oauth2/v1/userinfo|]

toLoginUser :: OktaUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }
