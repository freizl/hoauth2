{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Slack where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Slack = Slack deriving (Show, Eq)

type instance IDPUserInfo Slack = SlackUser

type instance IDPName Slack = Slack

slackIdp :: IDP Slack
slackIdp =
  def
    { idpName = Slack,
      oauth2Config = slackKey,
      convertUserInfoToLoginUser = toLoginUser,
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
