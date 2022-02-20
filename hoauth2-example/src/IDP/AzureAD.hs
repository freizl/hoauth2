{-# LANGUAGE DeriveGeneric #-}
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
import URI.ByteString
import URI.ByteString.QQ
import Utils

data AzureAD = AzureAD
  { oauth2 :: OAuth2,
    userInfoUri :: URI
  }
  deriving (Show, Generic, Eq)

azureADKey :: OAuth2
azureADKey =
  def
    { oauth2AuthorizeEndpoint =
        [uri|https://login.windows.net/common/oauth2/authorize|],
      oauth2TokenEndpoint =
        [uri|https://login.windows.net/common/oauth2/token|]
    }

init :: OAuth2 -> AzureAD
init key =
  AzureAD
    { oauth2 = key,
      userInfoUri = [uri|https://graph.microsoft.com/v1.0/me|]
    }

instance HasLabel AzureAD where
  idpLabel = const "AzureAD"

instance HasTokenRefreshReq AzureAD where
  tokenRefreshReq (AzureAD {..}) mgr = refreshAccessToken mgr oauth2

instance HasTokenReq AzureAD where
  tokenReq (AzureAD {..}) mgr = fetchAccessToken mgr oauth2

instance HasUserReq AzureAD where
  userReq (AzureAD {..}) mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri AzureAD where
  authUri (AzureAD {..}) =
    createCodeUri
      oauth2
      [ ("state", "AzureAD.test-state-123"),
        ("scope", "openid,profile"),
        ("resource", "https://graph.microsoft.com")
      ]

newtype AzureADUser = AzureADUser {mail :: Text} deriving (Show, Generic)

instance FromJSON AzureADUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: AzureADUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = mail ouser}
