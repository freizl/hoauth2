{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.ZOHO where
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

data ZOHO = ZOHO deriving (Show, Generic)

instance Hashable ZOHO

instance IDP ZOHO

instance HasLabel ZOHO

instance HasTokenReq ZOHO where
  tokenReq _ mgr = fetchAccessToken2 mgr zohoKey

instance HasTokenRefreshReq ZOHO where
  tokenRefreshReq _ mgr = refreshAccessToken2 mgr zohoKey

instance HasUserReq ZOHO where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri ZOHO where
  authUri _ = createCodeUri zohoKey [ ("state", "ZOHO.test-state-123")
                                    , ("scope", "ZohoCRM.users.READ")
                                    , ("access_type", "offline")
                                    , ("prompt", "consent")
                                    ]

data ZOHOUser = ZOHOUser { email    :: Text
                         , fullName :: Text
                         } deriving (Show, Generic)

newtype ZOHOUserResp = ZOHOUserResp { users :: [ZOHOUser] }
  deriving (Show, Generic)

instance FromJSON ZOHOUserResp where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON ZOHOUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://www.zohoapis.com/crm/v2/users|]
  -- `oauth/user/info` url does not work and find answer from
  -- https://help.zoho.com/portal/community/topic/oauth2-api-better-document-oauth-user-info

toLoginUser :: ZOHOUserResp -> LoginUser
toLoginUser resp =
  let us = users resp
  in
    case us of
      []    -> LoginUser { loginUserName = "no user found" }
      (a:_) -> LoginUser { loginUserName = fullName a }

