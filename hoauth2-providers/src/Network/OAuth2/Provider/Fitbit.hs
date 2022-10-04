{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.Fitbit where

import Control.Monad (mzero)
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Fitbit = Fitbit deriving (Eq, Show)

type instance IdpUserInfo Fitbit = FitbitUser

defaultFitbitApp :: IdpApplication 'AuthorizationCode Fitbit
defaultFitbitApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.empty,
      idpAppAuthorizeExtraParams = Map.empty,
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppName = "default-fitbit-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idp = defaultFitbitIdp
    }

defaultFitbitIdp :: Idp Fitbit
defaultFitbitIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Fitbit),
      idpUserInfoEndpoint = [uri|https://api.fitbit.com/1/user/-/profile.json|],
      idpAuthorizeEndpoint = [uri|https://www.fitbit.com/oauth2/authorize|],
      idpTokenEndpoint = [uri|https://api.fitbit.com/oauth2/token|]
    }

data FitbitUser = FitbitUser
  { userId :: Text,
    userName :: Text,
    userAge :: Int
  }
  deriving (Show, Eq)

instance FromJSON FitbitUser where
  parseJSON (Object o) =
    FitbitUser
      <$> ((o .: "user") >>= (.: "encodedId"))
      <*> ((o .: "user") >>= (.: "fullName"))
      <*> ((o .: "user") >>= (.: "age"))
  parseJSON _ = mzero
