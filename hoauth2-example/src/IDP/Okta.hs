{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Okta where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

newtype Okta = Okta IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

oktaIdp :: IDP
oktaIdp = IDP
      { idpName = "okta",
        oauth2Config = oktaKey,
        oauth2Scopes = ["openid", "profile", "offline_scope"],
        oauth2UserInfoUri = [uri|https://hw2.trexcloud.com/oauth2/v1/userinfo|]
      }

oktaKey :: OAuth2
oktaKey =
  def
    { oauth2AuthorizeEndpoint =
        [uri|https://hw2.trexcloud.com/oauth2/v1/authorize|],
      oauth2TokenEndpoint =
        [uri|https://hw2.trexcloud.com/oauth2/v1/token|]
    }

instance HasUserReq Okta where
  userReq (Okta IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

-- | https://developer.okta.com/docs/reference/api/oidc/#request-parameters
-- Okta Org AS doesn't support consent
-- Okta Custom AS does support consent via config (what scope shall prompt consent)

data OktaUser = OktaUser
  { name :: Text,
    preferredUsername :: Text
  }
  deriving (Show, Generic)

instance FromJSON OktaUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: OktaUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
