-- | [LinkedIn Authenticating with OAuth 2.0 Overview](https://learn.microsoft.com/en-us/linkedin/shared/authentication/authentication?context=linkedin%2Fcontext)
module Network.OAuth2.Provider.Linkedin where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Linkedin = Linkedin deriving (Eq, Show)

type instance IdpUserInfo Linkedin = LinkedinUser

defaultLinkedinApp :: AuthorizationCodeApplication
defaultLinkedinApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["r_liteprofile"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "default-linkedin-App"
    , acTokenRequestAuthenticationMethod = ClientSecretPost
    }

defaultLinkedinIdp :: Idp Linkedin
defaultLinkedinIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Linkedin)
    , idpUserInfoEndpoint = [uri|https://api.linkedin.com/v2/me|]
    , idpAuthorizeEndpoint = [uri|https://www.linkedin.com/oauth/v2/authorization|]
    , idpTokenEndpoint = [uri|https://www.linkedin.com/oauth/v2/accessToken|]
    }

data LinkedinUser = LinkedinUser
  { localizedFirstName :: Text
  , localizedLastName :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON LinkedinUser
