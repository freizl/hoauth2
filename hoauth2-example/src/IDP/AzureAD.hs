{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.AzureAD where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data AzureAD = AzureAD deriving (Eq, Show)

type instance IDPUserInfo AzureAD = AzureADUser

type instance IDPName AzureAD = AzureAD

azureIdp :: IDP AzureAD
azureIdp =
  def
    { idpName = AzureAD,
      oauth2Config = azureADKey,
      oauth2AuthorizeParams = [("resource", "https://graph.microsoft.com")],
      oauth2UserInfoUri = [uri|https://graph.microsoft.com/v1.0/me|],
      convertUserInfoToLoginUser = toLoginUser
    }

azureADKey :: OAuth2
azureADKey =
  def
    { oauth2AuthorizeEndpoint =
        [uri|https://login.windows.net/common/oauth2/authorize|],
      oauth2TokenEndpoint =
        [uri|https://login.windows.net/common/oauth2/token|]
    }

newtype AzureADUser = AzureADUser {mail :: Text} deriving (Show, Generic)

instance FromJSON AzureADUser where
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: AzureADUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = mail ouser}
