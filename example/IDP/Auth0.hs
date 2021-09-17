{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Auth0 where

import Data.Aeson
import Data.Bifunctor
import Data.Hashable
import Data.Text.Lazy (Text)
import GHC.Generics
import Keys
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

data Auth0 = Auth0
  deriving (Show, Generic)

instance Hashable Auth0

instance IDP Auth0

instance HasLabel Auth0

instance HasTokenReq Auth0 where
  tokenReq _ mgr = fetchAccessToken mgr auth0Key

instance HasTokenRefreshReq Auth0 where
  tokenRefreshReq _ mgr = refreshAccessToken mgr auth0Key

instance HasUserReq Auth0 where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Auth0 where
  authUri _ =
    createCodeUri
      auth0Key
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
