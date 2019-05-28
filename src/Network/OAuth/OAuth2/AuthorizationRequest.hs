{-# LANGUAGE DeriveGeneric #-}

module Network.OAuth.OAuth2.AuthorizationRequest where

import           Data.Aeson
import           Data.Aeson.Types (allNullaryToStringTag, camelTo2,
                                   constructorTagModifier)
import           GHC.Generics

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }
instance ToJSON Errors where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

-- | Authorization Code Grant Error Responses https://tools.ietf.org/html/rfc6749#section-4.1.2.1
-- Implicit Grant Error Responses https://tools.ietf.org/html/rfc6749#section-4.2.2.1
data Errors =
    InvalidRequest
  | UnauthorizedClient
  | AccessDenied
  | UnsupportedResponseType
  | InvalidScope
  | ServerError
  | TemporarilyUnavailable
  deriving (Show, Eq, Generic)
