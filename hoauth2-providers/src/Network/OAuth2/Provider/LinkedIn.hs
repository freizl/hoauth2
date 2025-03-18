{-# LANGUAGE QuasiQuotes #-}

-- | [LinkedIn Authenticating with OAuth 2.0 Overview](https://learn.microsoft.com/en-us/linkedin/shared/authentication/authentication?context=linkedin%2Fcontext)
module Network.OAuth2.Provider.LinkedIn where

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
import Network.URI.Static(uri)

sampleLinkedInAuthorizationCodeApp :: AuthorizationCodeApplication
sampleLinkedInAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-linkedin-authorization-code-app"
    , acClientAuthenticationMethod = ClientSecretPost
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

defaultLinkedInIdp :: Idp LinkedIn
defaultLinkedInIdp =
  Idp
    { idpUserInfoEndpoint = [uri|https://api.linkedin.com/v2/userinfo|]
    , idpAuthorizeEndpoint = [uri|https://www.linkedin.com/oauth/v2/authorization|]
    , idpTokenEndpoint = [uri|https://www.linkedin.com/oauth/v2/accessToken|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

data LinkedInUser = LinkedInUser
  { name :: Text
  , email :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON LinkedInUser
