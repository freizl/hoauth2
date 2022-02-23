{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.AzureAD where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype AzureAD = AzureAD IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

azureIdp :: IDP
azureIdp =
  IDP
    { idpName = "azure",
      oauth2Config = azureADKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://graph.microsoft.com/v1.0/me|]
    }

azureADKey :: OAuth2
azureADKey =
  def
    { oauth2AuthorizeEndpoint =
        [uri|https://login.windows.net/common/oauth2/authorize|],
      oauth2TokenEndpoint =
        [uri|https://login.windows.net/common/oauth2/token|]
    }

instance HasUserReq AzureAD where
  userReq (AzureAD IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

instance HasAuthorizeExtraParam AzureAD where
  authorizeParam _ = [("resource", "https://graph.microsoft.com")]

newtype AzureADUser = AzureADUser {mail :: Text} deriving (Show, Generic)

instance FromJSON AzureADUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: AzureADUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = mail ouser}
