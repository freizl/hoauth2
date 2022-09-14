{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.ZOHO where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

-- `oauth/user/info` url does not work and find answer from
-- https://help.zoho.com/portal/community/topic/oauth2-api-better-document-oauth-user-info

data ZOHO = ZOHO deriving (Eq, Show)

type instance IDPUserInfo ZOHO = ZOHOUserResp

type instance IDPName ZOHO = ZOHO

zohoIdp :: IDP ZOHO
zohoIdp =
  def
    { idpName = ZOHO,
      oauth2Config = zohoKey,
      oauth2AuthorizeParams = [("access_type", "offline"), ("prompt", "consent")],
      convertUserInfoToLoginUser = toLoginUser,
      oauth2FetchAccessToken = fetchAccessTokenWithAuthMethod ClientSecretPost,
      oauth2RefreshAccessToken = refreshAccessTokenWithAuthMethod ClientSecretPost,
      oauth2UserInfoUri = [uri|https://www.zohoapis.com/crm/v2/users|]
    }

zohoKey :: OAuth2
zohoKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://accounts.zoho.com/oauth/v2/auth|],
      oauth2TokenEndpoint = [uri|https://accounts.zoho.com/oauth/v2/token|]
    }

data ZOHOUser = ZOHOUser
  { email :: Text,
    fullName :: Text
  }
  deriving (Show, Generic)

newtype ZOHOUserResp = ZOHOUserResp {users :: [ZOHOUser]}
  deriving (Show, Generic)

instance FromJSON ZOHOUserResp where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON ZOHOUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: ZOHOUserResp -> LoginUser
toLoginUser resp =
  let us = users resp
   in case us of
        [] -> LoginUser {loginUserName = "ZOHO: no user found"}
        (a : _) -> LoginUser {loginUserName = fullName a}
