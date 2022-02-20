{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Okta where

import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils
import Data.Default

data Okta = Okta
  { oauth2 :: OAuth2,
    userInfoUri :: URI
  }
  deriving (Show, Generic, Eq)

oktaKey :: OAuth2
oktaKey =
  def
    { oauth2AuthorizeEndpoint =
        [uri|https://hw2.trexcloud.com/oauth2/v1/authorize|],
      oauth2TokenEndpoint =
        [uri|https://hw2.trexcloud.com/oauth2/v1/token|]
    }

-- | TODO: be able to take root domain and generate other uris
--
init :: OAuth2 -> Okta
init key =
  Okta
    { oauth2 = key,
      userInfoUri = [uri|https://hw2.trexcloud.com/oauth2/v1/userinfo|]
    }

instance HasLabel Okta where
  idpLabel = const "Okta"

instance HasTokenReq Okta where
  tokenReq Okta {..} mgr = fetchAccessToken mgr oauth2

instance HasTokenRefreshReq Okta where
  tokenRefreshReq Okta {..} mgr = refreshAccessToken mgr oauth2

instance HasUserReq Okta where
  userReq Okta {..} mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

-- | https://developer.okta.com/docs/reference/api/oidc/#request-parameters
-- Okta Org AS doesn't support consent
-- Okta Custom AS does support consent via config (what scope shall prompt consent)
instance HasAuthUri Okta where
  authUri Okta {..} =
    createCodeUri
      oauth2
      [ ("state", "Okta.test-state-123"),
        ( "scope",
          "openid profile offline_access"
          -- , "openid profile offline_access okta.users.read.self okta.users.read"
        ),
        ("prompt", "login consent")
      ]

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
