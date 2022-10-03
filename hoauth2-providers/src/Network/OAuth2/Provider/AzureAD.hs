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

defaultAzureADApp :: IdpApplication 'AuthorizationCode AzureAD
defaultAzureADApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.empty,
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppAuthorizeExtraParams = Map.fromList [("resource", "https://graph.microsoft.com")],
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppName = "default-azure-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idp = defaultAzureADIdp
    }

defaultAzureADIdp :: Idp AzureAD
defaultAzureADIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo AzureAD),
      idpUserInfoEndpoint = [uri|https://graph.microsoft.com/v1.0/me|],
      idpAuthorizeEndpoint = [uri|https://login.windows.net/common/oauth2/authorize|],
      idpTokenEndpoint = [uri|https://login.windows.net/common/oauth2/token|]
    }

newtype AzureADUser = AzureADUser {mail :: Text} deriving (Show, Generic)

instance FromJSON AzureADUser
