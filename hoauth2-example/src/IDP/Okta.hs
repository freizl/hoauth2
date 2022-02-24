{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Okta where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Okta = Okta deriving (Eq, Show)

type instance IDPUserInfo Okta = OktaUser
type instance IDPName Okta = Okta

oktaIdp :: IDP Okta
oktaIdp =
  def
    { idpName = Okta,
      oauth2Config = oktaKey,
      convertUserInfoToLoginUser = toLoginUser,
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
