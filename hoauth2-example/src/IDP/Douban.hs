{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Douban where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype Douban = Douban IDP
  deriving (HasLabel, HasAuthUri)

doubanIdp :: IDP
doubanIdp =
  IDP
    { idpName = "douban",
      oauth2Config = doubanKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.douban.com/v2/user/~me|]
    }

doubanKey :: OAuth2
doubanKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.douban.com/service/auth2/auth|],
      oauth2TokenEndpoint = [uri|https://www.douban.com/service/auth2/token|]
    }

instance HasTokenReq Douban where
  tokenReq (Douban IDP {..}) mgr = fetchAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasTokenRefreshReq Douban where
  tokenRefreshReq (Douban IDP {..}) mgr = refreshAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasUserReq Douban where
  userReq (Douban IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

data DoubanUser = DoubanUser
  { name :: Text,
    uid :: Text
  }
  deriving (Show, Generic)

instance FromJSON DoubanUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: DoubanUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
