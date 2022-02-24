{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Dropbox where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Dropbox = Dropbox deriving (Eq, Show)

type instance IDPUserInfo Dropbox = DropboxUser

type instance IDPName Dropbox = Dropbox

dropboxIdp :: IDP Dropbox
dropboxIdp =
  def
    { idpName = Dropbox,
      oauth2Config = dropboxKey,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2FetchUserInfo = fetchUserInfoViaPost,
      oauth2UserInfoUri = [uri|https://api.dropboxapi.com/2/users/get_current_account|]
    }

dropboxKey :: OAuth2
dropboxKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.dropbox.com/1/oauth2/authorize|],
      oauth2TokenEndpoint = [uri|https://api.dropboxapi.com/oauth2/token|]
    }

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
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: DropboxUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = displayName $ name ouser}
