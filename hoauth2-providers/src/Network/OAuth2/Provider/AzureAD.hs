{-# LANGUAGE QuasiQuotes #-}

-- | [AzureAD oauth2 flow](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow)
module Network.OAuth2.Provider.AzureAD where

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

data AzureAD = AzureAD deriving (Eq, Show)

type instance IdpUserInfo AzureAD = AzureADUser

-- Create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
defaultAzureADApp :: AuthorizationCodeApplication
defaultAzureADApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile", "email"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "default-azure-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

-- | https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration
defaultAzureADIdp :: MonadIO m => ExceptT Text m (Idp AzureAD)
defaultAzureADIdp = mkAzureIdp "common"

mkAzureIdp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @contoso.onmicrosoft.com@
  Text ->
  ExceptT Text m (Idp AzureAD)
mkAzureIdp domain = do
  OpenIDConfigurationUris {..} <- fetchWellKnownUris ("login.microsoftonline.com/" <> domain <> "/v2.0")
  pure $
    Idp
      { idpFetchUserInfo = authGetJSON @(IdpUserInfo AzureAD)
      , idpUserInfoEndpoint = userinfoUri
      , idpAuthorizeEndpoint = authorizationUri
      , idpTokenEndpoint = tokenUri
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
