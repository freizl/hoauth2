{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Dropbox where

import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Hashable
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

newtype Dropbox = Dropbox OAuth2 deriving (Show, Generic, Eq)

instance Hashable Dropbox

instance IDP Dropbox

instance HasLabel Dropbox where
  idpLabel = const "Dropbox"

instance HasTokenReq Dropbox where
  tokenReq (Dropbox key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Dropbox where
  tokenRefreshReq (Dropbox key) mgr = refreshAccessToken mgr key

instance HasUserReq Dropbox where
  userReq _ mgr at = do
    re <- authPostBS3 mgr at userInfoUri
    case eitherDecode re of
      Right obj -> return (toLoginUser obj)
      Left e -> throwE (BSL.pack e)

instance HasAuthUri Dropbox where
  authUri (Dropbox key) =
    createCodeUri
      key
      [ ("state", "Dropbox.test-state-123")
      ]

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

userInfoUri :: URI
userInfoUri = [uri|https://api.dropboxapi.com/2/users/get_current_account|]

toLoginUser :: DropboxUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = displayName $ name ouser}
