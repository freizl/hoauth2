{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Auth0 where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Auth0 = Auth0
  deriving (Show, Eq)

type instance IDPUserInfo Auth0 = Auth0User

type instance IDPName Auth0 = Auth0

auth0Idp :: IDP Auth0
auth0Idp =
  def
    { idpName = Auth0,
      oauth2Config = auth0Key,
      oauth2UserInfoUri = [uri|https://freizl.auth0.com/userinfo|],
      convertUserInfoToLoginUser = toLoginUser
    }

auth0Key :: OAuth2
auth0Key =
  def
    { oauth2AuthorizeEndpoint = [uri|https://freizl.auth0.com/authorize|],
      oauth2TokenEndpoint = [uri|https://freizl.auth0.com/oauth/token|]
    }

data Auth0User = Auth0User
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON Auth0User where
  parseJSON =
    genericParseJSON defaultOptions

toLoginUser :: Auth0User -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
