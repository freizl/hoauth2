{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module IDP.Facebook where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Text.Lazy                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           TokenUtil
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

data FacebookUser = FacebookUser { id    :: Text
                                 , name  :: Text
                                 , email :: Text
                                 } deriving (Show, Generic)

instance FromJSON FacebookUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://graph.facebook.com/me?fields=id,name,email|]

toLoginUser :: FacebookUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }

getUserInfo :: FromJSON a => Manager -> AccessToken -> IO (OAuth2Result a LoginUser)
getUserInfo mgr at = do
  re <- authGetJSON mgr at userInfoUri
  return (second toLoginUser re)

getAccessToken :: Manager
               -> OAuth2
               -> ExchangeToken
               -> IO (OAuth2Result TR.Errors OAuth2Token)
getAccessToken = postAT
