{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps
module Network.OAuth2.Provider.Github where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Github = Github deriving (Eq, Show)

type instance IdpUserInfo Github = GithubUser

defaultGithubApp :: IdpApplication 'AuthorizationCode Github
defaultGithubApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.empty,
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppAuthorizeExtraParams = Map.empty,
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppName = "default-github-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idp = defaultGithubIdp
    }

defaultGithubIdp :: Idp Github
defaultGithubIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Github),
      idpUserInfoEndpoint = [uri|https://api.github.com/user|],
      idpAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|],
      idpTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
    }

data GithubUser = GithubUser
  { name :: Text,
    id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON GithubUser 
