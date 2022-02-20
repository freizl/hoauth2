{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.ZOHO where

import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

userInfoUri :: URI
userInfoUri = [uri|https://www.zohoapis.com/crm/v2/users|]

-- `oauth/user/info` url does not work and find answer from
-- https://help.zoho.com/portal/community/topic/oauth2-api-better-document-oauth-user-info

newtype ZOHO = ZOHO OAuth2 deriving (Show, Generic, Eq)

instance HasLabel ZOHO where
  idpLabel = const "ZOHO"

instance HasTokenReq ZOHO where
  tokenReq (ZOHO key) mgr = fetchAccessTokenInternal ClientSecretPost mgr key

instance HasTokenRefreshReq ZOHO where
  tokenRefreshReq (ZOHO key) mgr = refreshAccessTokenInternal ClientSecretPost mgr key

instance HasUserReq ZOHO where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri ZOHO where
  authUri (ZOHO key) =
    createCodeUri
      key
      [ ("state", "ZOHO.test-state-123"),
        ("scope", "ZohoCRM.users.READ"),
        ("access_type", "offline"),
        ("prompt", "consent")
      ]

data ZOHOUser = ZOHOUser
  { email :: Text,
    fullName :: Text
  }
  deriving (Show, Generic)

newtype ZOHOUserResp = ZOHOUserResp {users :: [ZOHOUser]}
  deriving (Show, Generic)

instance FromJSON ZOHOUserResp where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON ZOHOUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: ZOHOUserResp -> LoginUser
toLoginUser resp =
  let us = users resp
   in case us of
        [] -> LoginUser {loginUserName = "no user found"}
        (a : _) -> LoginUser {loginUserName = fullName a}
