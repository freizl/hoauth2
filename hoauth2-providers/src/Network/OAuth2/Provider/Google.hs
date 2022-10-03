{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.Google where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

{-
To test at google playground, set redirect uri to "https://developers.google.com/oauthplayground"
-}

data Google = Google deriving (Eq, Show)

type instance IdpUserInfo Google = GoogleUser

defaultGoogleApp :: IdpApplication 'AuthorizationCode Google
defaultGoogleApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope =
        Set.fromList
          [ "https://www.googleapis.com/auth/userinfo.email",
            "https://www.googleapis.com/auth/userinfo.profile"
          ],
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppAuthorizeExtraParams = Map.empty,
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppName = "default-google-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idp = defaultGoogleIdp
    }

defaultGoogleIdp :: Idp Google
defaultGoogleIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Google),
      idpAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|],
      idpTokenEndpoint = [uri|https://oauth2.googleapis.com/token|],
      idpUserInfoEndpoint = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
    }

data GoogleUser = GoogleUser
  { -- | "scope": "https://www.googleapis.com/auth/userinfo.profile"]
    name :: Text,
    id :: Text,
    -- | "scopes": "https://www.googleapis.com/auth/userinfo.email",
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON GoogleUser
