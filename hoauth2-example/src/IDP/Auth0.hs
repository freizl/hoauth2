{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Auth0 where

import Data.Aeson
import Data.Bifunctor
import Data.Hashable
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

newtype Auth0 = Auth0 OAuth2
  deriving (Generic, Show, Eq)

instance Hashable Auth0

instance IDP Auth0

instance HasLabel Auth0 where
  idpLabel = const "Auth0"

instance HasTokenReq Auth0 where
  tokenReq (Auth0 key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Auth0 where
  tokenRefreshReq (Auth0 key) mgr = refreshAccessToken mgr key

instance HasUserReq Auth0 where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Auth0 where
  authUri (Auth0 key) =
    createCodeUri
      key
      [ ("state", "Auth0.test-state-123"),
        ( "scope",
          "openid profile email offline_access"
        )
      ]

data Auth0User = Auth0User
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON Auth0User where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

userInfoUri :: URI
userInfoUri = [uri|https://freizl.auth0.com/userinfo|]

toLoginUser :: Auth0User -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
