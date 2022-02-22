{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Dropbox where

import Data.Default
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype Dropbox = Dropbox IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

dropboxIdp :: IDP
dropboxIdp =
  IDP
    { idpName = "dropbox",
      oauth2Config = dropboxKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.dropboxapi.com/2/users/get_current_account|]
    }

dropboxKey :: OAuth2
dropboxKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.dropbox.com/1/oauth2/authorize|],
      oauth2TokenEndpoint = [uri|https://api.dropboxapi.com/oauth2/token|]
    }

instance HasUserReq Dropbox where
  userReq (Dropbox IDP{..}) mgr at = do
    re <- authPostJSON mgr at oauth2UserInfoUri []
    return (toLoginUser re)

newtype DropboxName = DropboxName {displayName :: Text}
  deriving (Show, Generic)

data DropboxUser = DropboxUser
  { email :: Text,
    name :: DropboxName
  }
  deriving (Show, Generic)

instance FromJSON DropboxName where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON DropboxUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: DropboxUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = displayName $ name ouser}
