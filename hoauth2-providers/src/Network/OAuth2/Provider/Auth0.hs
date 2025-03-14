{-# LANGUAGE QuasiQuotes #-}

-- | [Auth0](https://auth0.com)
--
--   * [Auth0 Authorize Application](https://auth0.com/docs/api/authentication#authorize-application)
--
--   * [OAuth 2.0 Authorization Framework](https://auth0.com/docs/authenticate/protocols/oauth)
module Network.OAuth2.Provider.Auth0 where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Network.OIDC.WellKnown
import URI.ByteString.QQ

sampleAuth0AuthorizationCodeApp :: AuthorizationCodeApplication
sampleAuth0AuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile", "email", "offline_access"]
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-auth0-authorization-code-app"
    , acAuthorizeRequestExtraParams = Map.empty
    , acClientAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

mkAuth0Idp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @foo.auth0.com@
  Text ->
  ExceptT Text m (Idp Auth0)
mkAuth0Idp domain = do
  OpenIDConfiguration {..} <- fetchWellKnown domain
  pure
    ( Idp
        { --  https://auth0.com/docs/api/authentication#user-profile
          idpUserInfoEndpoint = userinfoEndpoint
        , -- https://auth0.com/docs/api/authentication#authorization-code-flow
          idpAuthorizeEndpoint = authorizationEndpoint
        , -- https://auth0.com/docs/api/authentication#authorization-code-flow44
          idpTokenEndpoint = tokenEndpoint
        , idpDeviceAuthorizationEndpoint = Just deviceAuthorizationEndpoint
        }
    )

-- | https://auth0.com/docs/api/authentication#user-profile
data Auth0User = Auth0User
  { name :: Text
  , email :: Text
  , sub :: Text
  }
  deriving (Show, Generic)

instance FromJSON Auth0User
