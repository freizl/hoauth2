{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Fitbit where

import Control.Monad (mzero)
import Data.Default
import Data.Aeson
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype Fitbit = Fitbit IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

fitbitIdp :: IDP
fitbitIdp =
  IDP
    { idpName = "fitbit",
      oauth2Config = fitbitKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.fitbit.com/1/user/-/profile.json|]
    }

fitbitKey :: OAuth2
fitbitKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.fitbit.com/oauth2/authorize|],
      oauth2TokenEndpoint = [uri|https://api.fitbit.com/oauth2/token|]
    }

instance HasUserReq Fitbit where
  userReq (Fitbit IDP{..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

-- instance HasAuthUri Fitbit where
--   authUri (Fitbit key) =
--     createCodeUri
--       key
--       [ ("state", "Fitbit.test-state-123"),
--         ("scope", "profile")
--       ]

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
