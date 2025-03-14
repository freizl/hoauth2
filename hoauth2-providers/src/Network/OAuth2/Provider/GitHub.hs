{-# LANGUAGE QuasiQuotes #-}

-- | [Github build oauth applications guide](https://docs.github.com/en/developers/apps/building-oauth-apps)
module Network.OAuth2.Provider.GitHub where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString.QQ

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
    , acClientAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

defaultGithubIdp :: Idp GitHub
defaultGithubIdp =
  Idp
    { idpUserInfoEndpoint = [uri|https://api.github.com/user|]
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
