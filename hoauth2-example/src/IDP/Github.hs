{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Github where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

-- | http://developer.github.com/v3/oauth/
data Github = Github deriving (Eq, Show)

type instance IDPUserInfo Github = GithubUser

type instance IDPName Github = Github

githubIdp :: IDP Github
githubIdp =
  def
    { idpName = Github,
      oauth2Config = githubKey,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2UserInfoUri = [uri|https://api.github.com/user|]
    }

githubKey :: OAuth2
githubKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|],
      oauth2TokenEndpoint =
        [uri|https://github.com/login/oauth/access_token|]
    }

data GithubUser = GithubUser
  { name :: Text,
    id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON GithubUser where
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser {loginUserName = name guser}
