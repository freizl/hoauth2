{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.AzureAD where
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

data AzureAD = AzureAD OAuth2 deriving (Show, Generic, Eq)

instance Hashable AzureAD

instance IDP AzureAD

instance HasLabel AzureAD

instance HasTokenRefreshReq AzureAD where
  tokenRefreshReq (AzureAD key) mgr = refreshAccessToken mgr key

instance HasTokenReq AzureAD where
  tokenReq (AzureAD key) mgr = fetchAccessToken mgr key

instance HasUserReq AzureAD where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri AzureAD where
  authUri (AzureAD key) = createCodeUri key [ ("state", "AzureAD.test-state-123")
                                       , ("scope", "openid,profile")
                                       , ("resource", "https://graph.microsoft.com")
                                       ]

newtype AzureADUser = AzureADUser { mail :: Text } deriving (Show, Generic)

instance FromJSON AzureADUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://graph.microsoft.com/v1.0/me|]

toLoginUser :: AzureADUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = mail ouser }
