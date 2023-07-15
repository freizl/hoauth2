{-# LANGUAGE QuasiQuotes #-}

-- | [Github build oauth applications guide](https://docs.github.com/en/developers/apps/building-oauth-apps)
module Network.OAuth2.Provider.Github where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Github = Github deriving (Eq, Show)

type instance IdpUserInfo Github = GithubUser

defaultGithubApp :: AuthorizationCodeApplication
defaultGithubApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "default-github-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

defaultGithubIdp :: Idp Github
defaultGithubIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Github)
    , idpUserInfoEndpoint = [uri|https://api.github.com/user|]
    , idpAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
    , idpTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
    , idpDeviceAuthorizationEndpoint = Just [uri|https://github.com/login/device/code|]
    }

data GithubUser = GithubUser
  { name :: Text
  , id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON GithubUser
