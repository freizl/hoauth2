{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Okta where

import Data.Aeson
import Data.Bifunctor
import Data.Hashable
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

data Okta = Okta OAuth2
  deriving (Show, Generic, Eq)

userInfoUri :: URI
userInfoUri = [uri|https://hw2.trexcloud.com/oauth2/v1/userinfo|]
-- userInfoUri = [uri|https://dev-148986.oktapreview.com/oauth2/v1/userinfo|]

instance Hashable Okta

instance IDP Okta

instance HasLabel Okta

instance HasTokenReq Okta where
  tokenReq (Okta key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Okta where
  tokenRefreshReq (Okta key) mgr = refreshAccessToken mgr key

instance HasUserReq Okta where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

-- | https://developer.okta.com/docs/reference/api/oidc/#request-parameters
-- Okta Org AS doesn't support consent
-- Okta Custom AS does support consent via config (what scope shall prompt consent)
--
instance HasAuthUri Okta where
  authUri (Okta key) =
    createCodeUri
      key
      [ ("state", "Okta.test-state-123"),
        ( "scope",
          "openid profile"
          -- , "openid profile offline_access okta.users.read.self okta.users.read"
        ),
        ("prompt", "login consent")
      ]

data OktaUser = OktaUser
  { name :: Text,
    preferredUsername :: Text
  }
  deriving (Show, Generic)

instance FromJSON OktaUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: OktaUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
