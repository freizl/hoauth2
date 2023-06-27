{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | [Sign in with Slack](https://api.slack.com/authentication/sign-in-with-slack)
--
--   * [Using OAuth 2.0](https://api.slack.com/legacy/oauth)
module Network.OAuth2.Provider.Slack where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Slack = Slack deriving (Show, Eq)

type instance IdpUserInfo Slack = SlackUser

defaultSlackApp :: AuthorizationCodeApplication
defaultSlackApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    , acName = "default-slack-App"
    }

-- https://slack.com/.well-known/openid-configuration
defaultSlackIdp :: Idp Slack
defaultSlackIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Slack)
    , idpUserInfoEndpoint = [uri|https://slack.com/api/openid.connect.userInfo|]
    , idpAuthorizeEndpoint = [uri|https://slack.com/openid/connect/authorize|]
    , idpTokenEndpoint = [uri|https://slack.com/api/openid.connect.token|]
    }

data SlackUser = SlackUser
  { name :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON SlackUser
