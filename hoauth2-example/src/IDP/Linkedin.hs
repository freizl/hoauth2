{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-
https://docs.microsoft.com/en-us/linkedin/shared/authentication/authorization-code-flow?context=linkedin%2Fcontext&tabs=HTTPS
-}
module IDP.Linkedin where

import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

linkedinLabel :: Text
linkedinLabel = "LinkedIn"

newtype Linkedin = Linkedin OAuth2
  deriving (Generic, Show, Eq)

instance HasLabel Linkedin where
  idpLabel = const linkedinLabel

instance HasTokenReq Linkedin where
  tokenReq (Linkedin key) mgr = fetchAccessToken mgr key

-- | TODO: didn't find the right scope to obtain RefreshToken
instance HasTokenRefreshReq Linkedin where
  tokenRefreshReq (Linkedin key) mgr = refreshAccessToken mgr key

instance HasUserReq Linkedin where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri Linkedin where
  authUri (Linkedin key) =
    createCodeUri
      key
      [ ("state", tlToBS linkedinLabel <> ".test-state-123"),
        ("scope", "r_liteprofile")
      ]

data LinkedinUser = LinkedinUser
  { localizedFirstName :: Text,
    localizedLastName :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON LinkedinUser where
  parseJSON = genericParseJSON defaultOptions

userInfoUri :: URI
userInfoUri = [uri|https://api.linkedin.com/v2/me|]

toLoginUser :: LinkedinUser -> LoginUser
toLoginUser LinkedinUser {..} =
  LoginUser
    { loginUserName = localizedFirstName <> " " <> localizedLastName
    }
