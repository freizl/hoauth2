{-# LANGUAGE QuasiQuotes #-}

module Network.OAuth2.Provider.AzureAD where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data AzureAD = AzureAD deriving (Eq, Show)

type instance IdpUserInfo AzureAD = AzureADUser

-- create app at https://go.microsoft.com/fwlink/?linkid=2083908
--
-- also be aware to find the right client id.
-- see https://stackoverflow.com/a/70670961
defaultAzureADApp :: IdpApplication 'AuthorizationCode AzureAD
defaultAzureADApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.fromList ["openid", "profile", "email"]
    , idpAppAuthorizeState = "CHANGE_ME"
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppRedirectUri = [uri|http://localhost|]
    , idpAppName = "default-azure-app"
    , idpAppTokenRequestAuthenticationMethod = ClientSecretBasic
    , idp = defaultAzureADIdp
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
