{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.RefreshTokenRequest where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Types

-------------------------------------------------------------------------------
--                            RefreshToken Requset                           --
-------------------------------------------------------------------------------

data RefreshTokenRequest = RefreshTokenRequest
  { rrRefreshToken :: OAuth2.RefreshToken
  , rrGrantType :: GrantTypeValue
  , rrScope :: Set Scope
  , rrClientId :: Maybe ClientId
  , rrClientSecret :: Maybe ClientSecret
  }

instance ToQueryParam RefreshTokenRequest where
  toQueryParam :: RefreshTokenRequest -> Map Text Text
  toQueryParam RefreshTokenRequest {..} =
    Map.unions
      [ toQueryParam rrGrantType
      , toQueryParam rrScope
      , toQueryParam rrRefreshToken
      , toQueryParam rrClientId
      , toQueryParam rrClientSecret
      ]

class HasClientAuthenticationMethod a => HasRefreshTokenRequest a where
  -- | Make Refresh Token Request parameters
  -- | https://www.rfc-editor.org/rfc/rfc6749#section-6
  mkRefreshTokenRequestParam :: a -> OAuth2.RefreshToken -> RefreshTokenRequest
