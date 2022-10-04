{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.ZOHO where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

-- `oauth/user/info` url does not work and find answer from
-- https://help.zoho.com/portal/community/topic/oauth2-api-better-document-oauth-user-info

data ZOHO = ZOHO deriving (Eq, Show)

type instance IdpUserInfo ZOHO = ZOHOUserResp

defaultZohoApp :: IdpApplication 'AuthorizationCode ZOHO
defaultZohoApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.fromList ["ZohoCRM.users.READ"],
      idpAppAuthorizeExtraParams = Map.fromList [("access_type", "offline"), ("prompt", "consent")],
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppRedirectUri = [uri|http://localhost/oauth2/callback|],
      idpAppName = "default-zoho-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idp = defaultZohoIdp
    }

defaultZohoIdp :: Idp ZOHO
defaultZohoIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo ZOHO),
      idpUserInfoEndpoint = [uri|https://www.zohoapis.com/crm/v2/users|],
      idpAuthorizeEndpoint = [uri|https://accounts.zoho.com/oauth/v2/auth|],
      idpTokenEndpoint = [uri|https://accounts.zoho.com/oauth/v2/token|]
    }

data ZOHOUser = ZOHOUser
  { email :: Text,
    fullName :: Text
  }
  deriving (Show, Generic)

newtype ZOHOUserResp = ZOHOUserResp {users :: [ZOHOUser]}
  deriving (Show, Generic)

instance FromJSON ZOHOUserResp 

instance FromJSON ZOHOUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
