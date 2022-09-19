{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.Linkedin where

{-
https://docs.microsoft.com/en-us/linkedin/shared/authentication/authorization-code-flow?context=linkedin%2Fcontext&tabs=HTTPS
module Network.OAuth2.Provider.Linkedin where
-}

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Linkedin = Linkedin deriving (Eq, Show)

type instance IDPUserInfo Linkedin = LinkedinUser

defaultLinkedinApp :: IdpApplication 'AuthorizationCode Linkedin
defaultLinkedinApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.fromList ["r_liteprofile"],
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppAuthorizeExtraParams = Map.empty,
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppName = "default-linkedin-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretPost,
      idp = defaultLinkedinIdp
    }

defaultLinkedinIdp :: Idp Linkedin
defaultLinkedinIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IDPUserInfo Linkedin),
      idpUserInfoEndpoint = [uri|https://api.linkedin.com/v2/me|],
      idpAuthorizeEndpoint = [uri|https://www.linkedin.com/oauth/v2/authorization|],
      idpTokenEndpoint = [uri|https://www.linkedin.com/oauth/v2/accessToken|]
    }

data LinkedinUser = LinkedinUser
  { localizedFirstName :: Text,
    localizedLastName :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON LinkedinUser 
