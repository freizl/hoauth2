{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Facebook where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Facebook = Facebook deriving (Eq, Show)

type instance IDPUserInfo Facebook = FacebookUser

type instance IDPName Facebook = Facebook

facebookIdp :: IDP Facebook
facebookIdp =
  def
    { idpName = Facebook,
      oauth2Config = facebookKey,
      oauth2FetchAccessToken = fetchAccessTokenInternal ClientSecretPost,
      oauth2RefreshAccessToken = refreshAccessTokenInternal ClientSecretPost,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2UserInfoUri = [uri|https://graph.facebook.com/me?fields=id,name,email|]
    }

facebookKey :: OAuth2
facebookKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.facebook.com/dialog/oauth|],
      oauth2TokenEndpoint =
        [uri|https://graph.facebook.com/v2.3/oauth/access_token|]
    }

data FacebookUser = FacebookUser
  { id :: Text,
    name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON FacebookUser where
  parseJSON = genericParseJSON defaultOptions

toLoginUser :: FacebookUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
