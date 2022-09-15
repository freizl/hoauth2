-- | A lightweight oauth2 Haskell binding.
-- See Readme for more details
--
module Network.OAuth.OAuth2
  ( module Network.OAuth.OAuth2.HttpClient,
    module Network.OAuth.OAuth2.AuthorizationRequest,
    module Network.OAuth.OAuth2.TokenRequest,
    module Network.OAuth.OAuth2.Internal,
  )
where

{-
  Hiding Errors data type from default.
  Shall qualified import given the naming collision.
-}
import Network.OAuth.OAuth2.AuthorizationRequest hiding (Errors(..))
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2.Internal
import Network.OAuth.OAuth2.TokenRequest hiding (Errors(..))
