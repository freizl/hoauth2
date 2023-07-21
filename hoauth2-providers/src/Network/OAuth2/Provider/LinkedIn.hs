{-# LANGUAGE QuasiQuotes #-}

-- | [LinkedIn Authenticating with OAuth 2.0 Overview](https://learn.microsoft.com/en-us/linkedin/shared/authentication/authentication?context=linkedin%2Fcontext)
module Network.OAuth2.Provider.LinkedIn where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString.QQ

type instance IdpUserInfo LinkedIn = LinkedInUser

sampleLinkedInAuthorizationCodeApp :: AuthorizationCodeApplication
sampleLinkedInAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["r_liteprofile"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-linkedin-authorization-code-app"
    , acTokenRequestAuthenticationMethod = ClientSecretPost
    }

defaultLinkedInIdp :: Idp LinkedIn
defaultLinkedInIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo LinkedIn)
    , idpUserInfoEndpoint = [uri|https://api.linkedin.com/v2/me|]
    , idpAuthorizeEndpoint = [uri|https://www.linkedin.com/oauth/v2/authorization|]
    , idpTokenEndpoint = [uri|https://www.linkedin.com/oauth/v2/accessToken|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

data LinkedInUser = LinkedInUser
  { localizedFirstName :: Text
  , localizedLastName :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON LinkedInUser
