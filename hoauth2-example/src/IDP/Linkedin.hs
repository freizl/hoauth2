{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | TODO: didn't find the right scope to obtain RefreshToken
data Linkedin = Linkedin deriving (Eq, Show)

type instance IDPUserInfo Linkedin = LinkedinUser

type instance IDPName Linkedin = Linkedin

linkedinIdp :: IDP Linkedin
linkedinIdp =
  def
    { idpName = Linkedin,
      oauth2Config = linkedinKey,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2UserInfoUri = [uri|https://api.linkedin.com/v2/me|]
    }

linkedinKey :: OAuth2
linkedinKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://www.linkedin.com/oauth/v2/authorization|],
      oauth2TokenEndpoint = [uri|https://www.linkedin.com/oauth/v2/accessToken|]
    }

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
