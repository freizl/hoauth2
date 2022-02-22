{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Google where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
newtype Google = Google IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

googleIdp :: IDP
googleIdp =
  IDP
    { idpName = "google",
      oauth2Config = googleKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
    }

googleKey :: OAuth2
googleKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/auth|],
      oauth2TokenEndpoint = [uri|https://www.googleapis.com/oauth2/v3/token|]
    }

instance HasUserReq Google where
  userReq (Google IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

data GoogleUser = GoogleUser
  { name :: Text,
    id :: Text
  }
  deriving (Show, Generic)

instance FromJSON GoogleUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: GoogleUser -> LoginUser
toLoginUser guser = LoginUser {loginUserName = name guser}
