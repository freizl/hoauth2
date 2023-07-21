{-# LANGUAGE QuasiQuotes #-}

-- | [Sign in with Slack](https://api.slack.com/authentication/sign-in-with-slack)
--
--   * [Using OAuth 2.0](https://api.slack.com/legacy/oauth)
module Network.OAuth2.Provider.Slack where

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
import URI.ByteString.QQ (uri)

sampleSlackAuthorizationCodeApp :: AuthorizationCodeApplication
sampleSlackAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    , acName = "sample-slack-authorization-code-app"
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

-- https://slack.com/.well-known/openid-configuration
defaultSlackIdp :: Idp Slack
defaultSlackIdp =
  Idp
    { idpUserInfoEndpoint = [uri|https://slack.com/api/openid.connect.userInfo|]
    , idpAuthorizeEndpoint = [uri|https://slack.com/openid/connect/authorize|]
    , idpTokenEndpoint = [uri|https://slack.com/api/openid.connect.token|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

data SlackUser = SlackUser
  { name :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON SlackUser
