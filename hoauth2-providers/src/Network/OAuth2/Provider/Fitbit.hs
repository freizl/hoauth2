{-# LANGUAGE QuasiQuotes #-}

-- | [Fitbit Authorization developer guide](https://dev.fitbit.com/build/reference/web-api/developer-guide/authorization/)
module Network.OAuth2.Provider.Fitbit where

import Control.Monad (mzero)
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Fitbit = Fitbit deriving (Eq, Show)

type instance IdpUserInfo Fitbit = FitbitUser

sampleFitbitAuthorizationCodeApp :: AuthorizationCodeApplication
sampleFitbitAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeRequestExtraParams = Map.empty
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-fitbit-authorization-code-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

defaultFitbitIdp :: Idp Fitbit
defaultFitbitIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Fitbit)
    , idpUserInfoEndpoint = [uri|https://api.fitbit.com/1/user/-/profile.json|]
    , idpAuthorizeEndpoint = [uri|https://www.fitbit.com/oauth2/authorize|]
    , idpTokenEndpoint = [uri|https://api.fitbit.com/oauth2/token|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

data FitbitUser = FitbitUser
  { userId :: Text
  , userName :: Text
  , userAge :: Int
  }
  deriving (Show, Eq)

instance FromJSON FitbitUser where
  parseJSON (Object o) =
    FitbitUser
      <$> ((o .: "user") >>= (.: "encodedId"))
      <*> ((o .: "user") >>= (.: "fullName"))
      <*> ((o .: "user") >>= (.: "age"))
  parseJSON _ = mzero
