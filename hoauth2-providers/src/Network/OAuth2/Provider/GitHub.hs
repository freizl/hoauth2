{-# LANGUAGE QuasiQuotes #-}

-- | [Github build oauth applications guide](https://docs.github.com/en/developers/apps/building-oauth-apps)
module Network.OAuth2.Provider.GitHub where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString.QQ

type instance IdpUserInfo GitHub = GitHubUser

sampleGithubAuthorizationCodeApp :: AuthorizationCodeApplication
sampleGithubAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-github-authorization-code-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

defaultGithubIdp :: Idp GitHub
defaultGithubIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo GitHub)
    , idpUserInfoEndpoint = [uri|https://api.github.com/user|]
    , idpAuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|]
    , idpTokenEndpoint = [uri|https://github.com/login/oauth/access_token|]
    , idpDeviceAuthorizationEndpoint = Just [uri|https://github.com/login/device/code|]
    }

data GitHubUser = GitHubUser
  { name :: Text
  , id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON GitHubUser
