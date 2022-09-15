{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings Authorization part of The OAuth 2.0 Authorization Framework
-- RFC6749 <https://www.rfc-editor.org/rfc/rfc6749>
module Network.OAuth.OAuth2.AuthorizationRequest where

import Data.Aeson
import qualified Data.Text.Encoding as T
import GHC.Generics
import Lens.Micro
import Network.OAuth.OAuth2.Internal
import URI.ByteString

--------------------------------------------------

-- * Errors

--------------------------------------------------

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}

instance ToJSON Errors where
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}

-- | Authorization Code Grant Error Responses https://tools.ietf.org/html/rfc6749#section-4.1.2.1
-- Implicit Grant Error Responses https://tools.ietf.org/html/rfc6749#section-4.2.2.1
data Errors
  = InvalidRequest
  | UnauthorizedClient
  | AccessDenied
  | UnsupportedResponseType
  | InvalidScope
  | ServerError
  | TemporarilyUnavailable
  deriving (Show, Eq, Generic)

--------------------------------------------------

-- * URLs

--------------------------------------------------

-- | Prepare the authorization URL.  Redirect to this URL
-- asking for user interactive authentication.
authorizationUrl :: OAuth2 -> URI
authorizationUrl oa = over (queryL . queryPairsL) (++ queryParts) (oauth2AuthorizeEndpoint oa)
  where
    queryParts =
      [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa),
        ("response_type", "code"),
        ("redirect_uri", serializeURIRef' $ oauth2RedirectUri oa)
      ]
