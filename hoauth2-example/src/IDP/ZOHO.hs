{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

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

newtype ZOHO = ZOHO IDP
  deriving (HasLabel, HasAuthUri)

zohoIdp :: IDP
zohoIdp =
  IDP
    { idpName = "zoho",
      oauth2Config = zohoKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://www.zohoapis.com/crm/v2/users|]
    }

zohoKey :: OAuth2
zohoKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://accounts.zoho.com/oauth/v2/auth|],
      oauth2TokenEndpoint = [uri|https://accounts.zoho.com/oauth/v2/token|]
    }

instance HasTokenReq ZOHO where
  tokenReq (ZOHO IDP{..}) mgr = fetchAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasTokenRefreshReq ZOHO where
  tokenRefreshReq (ZOHO IDP{..}) mgr = refreshAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasUserReq ZOHO where
  userReq (ZOHO IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

instance HasAuthorizeExtraParam ZOHO where
  authorizeParam _ =
    [ ("access_type", "offline"),
      ("prompt", "consent")
    ]

-- instance HasAuthUri ZOHO where
--   authUri (ZOHO key) =
--     createCodeUri
--       key
--       [ ("state", "ZOHO.test-state-123"),
--         ("scope", "ZohoCRM.users.READ"),
--         ("access_type", "offline"),
--         ("prompt", "consent")
--       ]

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
