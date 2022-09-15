{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Google where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

-- | Test at google playground
-- oauthCallback = Just "https://developers.google.com/oauthplayground"
--
data Google = Google deriving (Eq,Show)
type instance IDPUserInfo Google = GoogleUser
type instance IDPName Google = Google

googleIdp :: IDP Google
googleIdp =
  def
    { idpName = Google,
      oauth2Config = googleKey,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2UserInfoUri = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
    }

googleKey :: OAuth2
googleKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/auth|],
      oauth2TokenEndpoint = [uri|https://www.googleapis.com/oauth2/v3/token|]
    }


data GoogleUser = GoogleUser
  { name :: Text,
    id :: Text
  }
  deriving (Show, Generic)

instance FromJSON GoogleUser where
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: GoogleUser -> LoginUser
toLoginUser guser = LoginUser {loginUserName = name guser}
