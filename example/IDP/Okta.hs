{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Okta where
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Keys
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

data Okta = Okta deriving (Show, Generic)

instance Hashable Okta

instance IDP Okta

instance HasLabel Okta

instance HasTokenReq Okta where
  tokenReq _ mgr = fetchAccessToken mgr oktaKey

instance HasTokenRefreshReq Okta where
  tokenRefreshReq _ mgr rt = refreshAccessToken mgr oktaKey rt

instance HasUserReq Okta where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Okta where
  authUri _ = createCodeUri oktaKey [ ("state", "Okta.test-state-123")
                                    , ("scope", "openid profile offline_access")
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

