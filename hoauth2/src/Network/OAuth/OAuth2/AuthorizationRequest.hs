{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bindings Authorization part of The OAuth 2.0 Authorization Framework
-- RFC6749 <https://www.rfc-editor.org/rfc/rfc6749>
module Network.OAuth.OAuth2.AuthorizationRequest where

import Data.Aeson
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Lens.Micro (over)
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
-- I found hard time to figure a way to test the authorization error flow
-- When anything wrong in @/authorize@ request (redirect to OAuth2 provider),
-- it will end-up at the Provider page hence no way for this library to parse error response.
-- In other words, @/authorize@ ends up with 4xx or 5xx.
-- Revisit this whenever find a case OAuth2 provider redirects back to Relying party with errors.
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

-- | See 'authorizationUrlWithParams'
authorizationUrl :: OAuth2 -> URI
authorizationUrl = authorizationUrlWithParams []

-- | Prepare the authorization URL.  Redirect to this URL
-- asking for user interactive authentication.
--
-- @since 2.6.0
authorizationUrlWithParams :: QueryParams -> OAuth2 -> URI
authorizationUrlWithParams qs oa = over (queryL . queryPairsL) (++ queryParts) (oauth2AuthorizeEndpoint oa)
  where
    queryParts =
      List.nubBy ((==) `on` fst) $
        qs
          ++ [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa),
               ("response_type", "code"),
               ("redirect_uri", serializeURIRef' $ oauth2RedirectUri oa)
             ]
