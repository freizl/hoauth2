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
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ
import Network.Google.OAuth2.JWT (SignedJWT)

{-
To test at google playground, set redirect uri to "https://developers.google.com/oauthplayground"
-}

data Google = Google deriving (Eq, Show)

type instance IdpUserInfo Google = GoogleUser

-- * Authorization Code flow

defaultGoogleApp :: IdpApplication 'AuthorizationCode Google
defaultGoogleApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope =
        Set.fromList
          [ "https://www.googleapis.com/auth/userinfo.email"
          , "https://www.googleapis.com/auth/userinfo.profile"
          ]
    , idpAppAuthorizeState = "CHANGE_ME"
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppRedirectUri = [uri|http://localhost|]
    , idpAppName = "default-google-App"
    , idpAppTokenRequestAuthenticationMethod = ClientSecretBasic
    , idp = defaultGoogleIdp
    }

-- * Service Account

-- | Service account key (in JSON format) that download from google
data GoogleServiceAccountKey = GoogleServiceAccountKey
  { privateKey :: String
  , clientEmail :: Text
  , projectId :: Text
  , privateKeyId :: Text
  , clientId :: Text
  , authUri :: Text
  , tokenUri :: Text
  , authProviderX509CertUrl :: Text
  , clientX509CertUrl :: Text
  } deriving (Generic)

instance FromJSON GoogleServiceAccountKey where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

defaultServiceAccountApp :: SignedJWT -> IdpApplication 'JwtBearer Google
defaultServiceAccountApp jwt =
  JwtBearerIdpApplication
    { idpAppName = "google-sa-app"
    , idpAppJwt = (BS8.pack $ show jwt)
    , idp = defaultGoogleIdp
    }

-- * IDP

defaultGoogleIdp :: Idp Google
defaultGoogleIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Google)
    , idpAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|]
    , idpTokenEndpoint = [uri|https://oauth2.googleapis.com/token|]
    , idpUserInfoEndpoint = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
    }

-- requires scope "https://www.googleapis.com/auth/userinfo.profile" to obtain "name".
-- requires scopes "https://www.googleapis.com/auth/userinfo.email" to obtain "email".
data GoogleUser = GoogleUser
  { name :: Text
  , id :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON GoogleUser
