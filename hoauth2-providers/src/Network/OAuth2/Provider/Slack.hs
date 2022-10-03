{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.Slack where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Slack = Slack deriving (Show, Eq)

type instance IdpUserInfo Slack = SlackUser

defaultSlackApp :: IdpApplication 'AuthorizationCode Slack
defaultSlackApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.fromList ["openid", "profile"],
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppAuthorizeExtraParams = Map.empty,
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idpAppName = "default-slack-App",
      idp = defaultSlackIdp
    }

-- https://api.slack.com/authentication/sign-in-with-slack
-- https://slack.com/.well-known/openid-configuration
defaultSlackIdp :: Idp Slack
defaultSlackIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Slack),
      idpUserInfoEndpoint = [uri|https://slack.com/api/openid.connect.userInfo|],
      idpAuthorizeEndpoint = [uri|https://slack.com/openid/connect/authorize|],
      idpTokenEndpoint = [uri|https://slack.com/api/openid.connect.token|]
    }

data SlackUser = SlackUser
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON SlackUser
