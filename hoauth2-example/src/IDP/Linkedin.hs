{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-
https://docs.microsoft.com/en-us/linkedin/shared/authentication/authorization-code-flow?context=linkedin%2Fcontext&tabs=HTTPS
-}
module IDP.Linkedin where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

linkedinLabel :: Text
linkedinLabel = "LinkedIn"

-- | TODO: didn't find the right scope to obtain RefreshToken
newtype Linkedin = Linkedin IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

linkedinIdp :: IDP
linkedinIdp =
  IDP
    { idpName = "linkedin",
      oauth2Config = linkedinKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.linkedin.com/v2/me|]
    }

linkedinKey :: OAuth2
linkedinKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.linkedin.com/oauth/v2/authorization|],
      oauth2TokenEndpoint = [uri|https://www.linkedin.com/oauth/v2/accessToken|]
    }

instance HasUserReq Linkedin where
  userReq (Linkedin IDP {..}) mgr at = do
    re <- authGetJSON mgr at oauth2UserInfoUri
    return (toLoginUser re)

data LinkedinUser = LinkedinUser
  { localizedFirstName :: Text,
    localizedLastName :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON LinkedinUser where
  parseJSON = genericParseJSON defaultOptions



toLoginUser :: LinkedinUser -> LoginUser
toLoginUser LinkedinUser {..} =
  LoginUser
    { loginUserName = localizedFirstName <> " " <> localizedLastName
    }
