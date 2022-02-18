{-# LANGUAGE DeriveGeneric #-}

module Network.OAuth.OAuth2.TokenRequest where

import           Data.Aeson
import           GHC.Generics

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }
instance ToJSON Errors where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

-- | Token Error Responses https://tools.ietf.org/html/rfc6749#section-5.2
data Errors =
    InvalidRequest
  | InvalidClient
  | InvalidGrant
  | UnauthorizedClient
  | UnsupportedGrantType
  | InvalidScope
  deriving (Show, Eq, Generic)
