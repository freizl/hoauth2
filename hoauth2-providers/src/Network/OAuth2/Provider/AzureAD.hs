{-# LANGUAGE QuasiQuotes #-}

-- | [AzureAD oauth2 flow](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow)
module Network.OAuth2.Provider.AzureAD where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OAuth2.Experiment.GrantType.AuthorizationCode qualified as AuthorizationCode
import URI.ByteString.QQ

data AzureAD = AzureAD deriving (Eq, Show)

type instance IdpUserInfo AzureAD = AzureADUser

-- create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
defaultAzureADApp :: AuthorizationCode.Application
defaultAzureADApp =
  AuthorizationCode.Application
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
defaultAzureADIdp :: Idp AzureAD
defaultAzureADIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo AzureAD)
    , idpUserInfoEndpoint = [uri|https://graph.microsoft.com/oidc/userinfo|]
    , idpAuthorizeEndpoint = [uri|https://login.microsoftonline.com/common/oauth2/v2.0/authorize|]
    , idpTokenEndpoint = [uri|https://login.microsoftonline.com/common/oauth2/v2.0/token|]
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
