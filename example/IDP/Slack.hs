{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Slack where

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

newtype Slack = Slack OAuth2
  deriving (Show, Generic, Eq)

instance Hashable Slack

instance IDP Slack

instance HasLabel Slack where
  idpLabel = const "Slack"

instance HasTokenReq Slack where
  tokenReq (Slack key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Slack where
  tokenRefreshReq (Slack key) mgr = refreshAccessToken mgr key

instance HasUserReq Slack where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Slack where
  authUri (Slack key) =
    createCodeUri
      key
      [ ("state", "Slack.test-state-123"),
        ( "scope",
          "openid profile email"
        )
      ]

data SlackUser = SlackUser
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON SlackUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

userInfoUri :: URI
userInfoUri = [uri|https://slack.com/api/openid.connect.userInfo|]

toLoginUser :: SlackUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
