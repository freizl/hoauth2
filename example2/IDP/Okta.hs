{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE OverloadedStrings #-}

module IDP.Okta where
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Text.Lazy                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Hashable
import Keys
import Utils

data Okta = Okta deriving (Show, Generic)

instance Hashable Okta

instance IDP Okta

instance HasLabel Okta

instance HasTokenReq Okta where
  tokenReq _ mgr code = fetchAccessToken2 mgr oktaKey code

instance HasUserReq Okta where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Okta where
  authUri _ = createCodeUri oktaKey [ ("state", "Okta.test-state-123")
                                        , ("scope", "user_about_me,email")
                                        ] 

data OktaUser = OktaUser { name              :: Text
                         , preferredUsername :: Text
                         } deriving (Show, Generic)

instance FromJSON OktaUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://dev-148986.oktapreview.com/oauth2/v1/userinfo|]

toLoginUser :: OktaUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }

getUserInfo :: FromJSON a => Manager -> AccessToken -> IO (OAuth2Result a LoginUser)
getUserInfo mgr at = do
  re <- authGetJSON mgr at userInfoUri
  return (second toLoginUser re)

getAccessToken :: Manager
               -> OAuth2
               -> ExchangeToken
               -> IO (OAuth2Result TR.Errors OAuth2Token)
getAccessToken = fetchAccessToken
