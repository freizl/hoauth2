{-# LANGUAGE QuasiQuotes #-}

-- | [AzureAD oauth2 flow](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow)
module Network.OAuth2.Provider.AzureAD where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.HTTP.Conduit (Manager)
import Network.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Network.OIDC.WellKnown
import URI.ByteString.QQ

-- Create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
sampleAzureADAuthorizationCodeApp :: AuthorizationCodeApplication
sampleAzureADAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile", "email"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-azure-authorization-code-app"
    , acClientAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

-- | https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration
-- It's supporse to resue 'mkAzureIdp'
--
-- @
-- mkAzureIdp "common"
-- @
--
-- But its issuer is "https://login.microsoftonline.com/{tenantid}/v2.0",
-- which is invalid URI!!
defaultAzureADIdp :: Idp AzureAD
defaultAzureADIdp =
  Idp
    { idpAuthorizeEndpoint = [uri|https://login.microsoftonline.com/common/oauth2/v2.0/authorize|]
    , idpTokenEndpoint = [uri|https://login.microsoftonline.com/common/oauth2/v2.0/token|]
    , idpUserInfoEndpoint = [uri|https://graph.microsoft.com/oidc/userinfo|]
    , idpDeviceAuthorizationEndpoint = Just [uri|https://login.microsoftonline.com/common/oauth2/v2.0/devicecode|]
    }

mkAzureIdp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @contoso.onmicrosoft.com@
  Text ->
  ExceptT Text m (Idp AzureAD)
mkAzureIdp domain = do
  OpenIDConfiguration {..} <- fetchWellKnown ("login.microsoftonline.com/" <> domain <> "/v2.0")
  pure $
    Idp
      { idpUserInfoEndpoint = userinfoEndpoint
      , idpAuthorizeEndpoint = authorizationEndpoint
      , idpTokenEndpoint = tokenEndpoint
      , idpDeviceAuthorizationEndpoint = Just deviceAuthorizationEndpoint
      }

-- | https://learn.microsoft.com/en-us/azure/active-directory/develop/userinfo
data AzureADUser = AzureADUser
  { sub :: Text
  , email :: Text
  , familyName :: Text
  , givenName :: Text
  , name :: Text
  }
  deriving (Show, Generic)

instance FromJSON AzureADUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
