{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Facebook where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype Facebook = Facebook IDP
  deriving (HasLabel, HasAuthUri)

facebookIdp :: IDP
facebookIdp =
  IDP
    { idpName = "facebook",
      oauth2Config = facebookKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://graph.facebook.com/me?fields=id,name,email|]
    }

facebookKey :: OAuth2
facebookKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.facebook.com/dialog/oauth|],
      oauth2TokenEndpoint =
        [uri|https://graph.facebook.com/v2.3/oauth/access_token|]
    }

instance HasTokenReq Facebook where
  tokenReq (Facebook IDP{..}) mgr = fetchAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasTokenRefreshReq Facebook where
  tokenRefreshReq (Facebook IDP{..}) mgr = refreshAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasUserReq Facebook where
  userReq (Facebook IDP{..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

data FacebookUser = FacebookUser
  { id :: Text,
    name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON FacebookUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: FacebookUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
