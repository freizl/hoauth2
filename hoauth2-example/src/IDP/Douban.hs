{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Douban where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Douban = Douban deriving (Eq, Show)

type instance IDPUserInfo Douban = DoubanUser

type instance IDPName Douban = Douban

doubanIdp :: IDP Douban
doubanIdp =
  def
    { idpName = Douban,
      oauth2Config = doubanKey,
      oauth2UserInfoUri = [uri|https://api.douban.com/v2/user/~me|],
      oauth2FetchAccessToken = fetchAccessTokenInternal ClientSecretPost,
      oauth2RefreshAccessToken = refreshAccessTokenInternal ClientSecretPost,
      convertUserInfoToLoginUser = toLoginUser
    }

doubanKey :: OAuth2
doubanKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.douban.com/service/auth2/auth|],
      oauth2TokenEndpoint = [uri|https://www.douban.com/service/auth2/token|]
    }

data DoubanUser = DoubanUser
  { name :: Text,
    uid :: Text
  }
  deriving (Show, Generic)

instance FromJSON DoubanUser where
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: DoubanUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
