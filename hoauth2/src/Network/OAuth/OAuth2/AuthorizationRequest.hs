-- | Bindings Authorization part of The OAuth 2.0 Authorization Framework
-- RFC6749 <https://www.rfc-editor.org/rfc/rfc6749>
module Network.OAuth.OAuth2.AuthorizationRequest where

import Data.Aeson
import Data.Function (on)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Lens.Micro (over)
import Network.OAuth.OAuth2.Internal
import URI.ByteString
import Prelude hiding (error)

--------------------------------------------------

-- * Authorization Request Errors

--------------------------------------------------

-- | Authorization Code Grant Error Responses https://tools.ietf.org/html/rfc6749#section-4.1.2.1
--
-- I found hard time to figure a way to test the authorization error flow
-- When anything wrong in @/authorize@ request, it will stuck at the Provider page
-- hence no way for this library to parse error response.
-- In other words, @/authorize@ ends up with 4xx or 5xx.
-- Revisit this whenever find a case OAuth2 provider redirects back to Relying party with errors.
data AuthorizationResponseError = AuthorizationResponseError
  { authorizationResponseError :: AuthorizationResponseErrorCode
  , authorizationResponseErrorDescription :: Maybe Text
  , authorizationResponseErrorUri :: Maybe (URIRef Absolute)
  }
  deriving (Show, Eq)

data AuthorizationResponseErrorCode
  = InvalidRequest
  | UnauthorizedClient
  | AccessDenied
  | UnsupportedResponseType
  | InvalidScope
  | ServerError
  | TemporarilyUnavailable
  | UnknownErrorCode Text
  deriving (Show, Eq)

instance FromJSON AuthorizationResponseErrorCode where
  parseJSON = withText "parseJSON AuthorizationResponseErrorCode" $ \t ->
    pure $ case t of
      "invalid_request" -> InvalidRequest
      "unauthorized_client" -> UnauthorizedClient
      "access_denied" -> AccessDenied
      "unsupported_response_type" -> UnsupportedResponseType
      "invalid_scope" -> InvalidScope
      "server_error" -> ServerError
      "temporarily_unavailable" -> TemporarilyUnavailable
      _ -> UnknownErrorCode t

instance FromJSON AuthorizationResponseError where
  parseJSON = withObject "parseJSON AuthorizationResponseError" $ \t -> do
    authorizationResponseError <- t .: "error"
    authorizationResponseErrorDescription <- t .:? "error_description"
    authorizationResponseErrorUri <- t .:? "error_uri"
    pure AuthorizationResponseError {..}

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
          ++ [ ("client_id", T.encodeUtf8 $ oauth2ClientId oa)
             , ("response_type", "code")
             , ("redirect_uri", serializeURIRef' $ oauth2RedirectUri oa)
             ]
