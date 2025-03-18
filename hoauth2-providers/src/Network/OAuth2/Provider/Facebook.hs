{-# LANGUAGE QuasiQuotes #-}

-- | [Facebook Login](http://developers.facebook.com/docs/facebook-login/)
module Network.OAuth2.Provider.Facebook where

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

sampleFacebookAuthorizationCodeApp :: AuthorizationCodeApplication
sampleFacebookAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["public_profile", "email"]
    , acAuthorizeRequestExtraParams = Map.empty
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-facebook-authorization-code-app"
    , acClientAuthenticationMethod = ClientSecretPost
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

defaultFacebookIdp :: Idp Facebook
defaultFacebookIdp =
  Idp
    { idpUserInfoEndpoint = [uri|https://graph.facebook.com/me?fields=id,name,email|]
    , idpAuthorizeEndpoint = [uri|https://www.facebook.com/dialog/oauth|]
    , idpTokenEndpoint = [uri|https://graph.facebook.com/v22.0/oauth/access_token|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

data FacebookUser = FacebookUser
  { id :: Text
  , name :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON FacebookUser
