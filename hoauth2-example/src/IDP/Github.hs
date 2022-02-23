{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Github where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

-- | http://developer.github.com/v3/oauth/
newtype Github = Github IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

githubIdp :: IDP
githubIdp =
  IDP
    { idpName = "github",
      oauth2Config = githubKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.github.com/user|]
    }

githubKey :: OAuth2
githubKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://github.com/login/oauth/authorize|],
      oauth2TokenEndpoint =
        [uri|https://github.com/login/oauth/access_token|]
    }

instance HasUserReq Github where
  userReq (Github IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

data GithubUser = GithubUser
  { name :: Text,
    id :: Integer
  }
  deriving (Show, Generic)

instance FromJSON GithubUser where
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser {loginUserName = name guser}
