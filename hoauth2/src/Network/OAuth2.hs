-- | A lightweight oauth2 Haskell binding.
-- See Readme for more details
module Network.OAuth2 (
  module Network.OAuth2.Internal,

  -- * Authorization Requset
  module Network.OAuth2.AuthorizationRequest,

  -- * Token Request
  module Network.OAuth2.TokenRequest,

  -- * OAuth'ed http client utilities
  module Network.OAuth2.HttpClient,
) where

{-
  Hiding Errors data type from default.
  Shall qualified import given the naming collision.
-}
import Network.OAuth2.AuthorizationRequest hiding (
  AuthorizationResponseError (..),
  AuthorizationResponseErrorCode (..),
 )
import Network.OAuth2.HttpClient
import Network.OAuth2.Internal
import Network.OAuth2.TokenRequest
