{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Fitbit where

import Control.Monad (mzero)
import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Fitbit = Fitbit deriving (Eq, Show)

type instance IDPUserInfo Fitbit = FitbitUser

type instance IDPName Fitbit = Fitbit

fitbitIdp :: IDP Fitbit
fitbitIdp =
  def
    { idpName = Fitbit,
      oauth2Config = fitbitKey,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2UserInfoUri = [uri|https://api.fitbit.com/1/user/-/profile.json|]
    }

fitbitKey :: OAuth2
fitbitKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.fitbit.com/oauth2/authorize|],
      oauth2TokenEndpoint = [uri|https://api.fitbit.com/oauth2/token|]
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

toLoginUser :: FitbitUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = userName ouser}
