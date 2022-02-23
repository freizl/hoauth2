{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Slack where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype Slack = Slack IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

slackIdp :: IDP
slackIdp =
  IDP
    { idpName = "slack",
      oauth2Config = slackKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://slack.com/api/openid.connect.userInfo|]
    }

-- https://api.slack.com/authentication/sign-in-with-slack
-- https://slack.com/.well-known/openid-configuration
slackKey :: OAuth2
slackKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://slack.com/openid/connect/authorize|],
      oauth2TokenEndpoint = [uri|https://slack.com/api/openid.connect.token|]
    }

instance HasUserReq Slack where
  userReq (Slack IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

data SlackUser = SlackUser
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON SlackUser where
  parseJSON =
    genericParseJSON defaultOptions

toLoginUser :: SlackUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
