{-# LANGUAGE QuasiQuotes #-}

-- | [Auth0](https://auth0.com)
--
--   * [Auth0 Authorize Application](https://auth0.com/docs/api/authentication#authorize-application)
--
--   * [OAuth 2.0 Authorization Framework](https://auth0.com/docs/authenticate/protocols/oauth)
module Network.OAuth2.Provider.Auth0 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OIDC.WellKnown
import URI.ByteString.QQ

-- data Auth0 = Auth0
--   deriving (Show, Eq)

type instance IdpUserInfo Auth0 = Auth0User

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
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

mkAuth0Idp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @foo.auth0.com@
  Text ->
  ExceptT Text m (Idp Auth0)
mkAuth0Idp domain = do
  OpenIDConfiguration {..} <- fetchWellKnown domain
  pure
    ( Idp
        { idpFetchUserInfo = authGetJSON @(IdpUserInfo Auth0)
        , --  https://auth0.com/docs/api/authentication#user-profile
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
