{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.RefreshTokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils

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

class (HasOAuth2Key a, HasTokenRequestClientAuthenticationMethod a) => HasRefreshTokenRequest a where
  -- | Make Refresh Token Request parameters
  -- | https://www.rfc-editor.org/rfc/rfc6749#section-6
  mkRefreshTokenRequestParam :: a -> OAuth2.RefreshToken -> RefreshTokenRequest

-- | Make Refresh Token Request
-- https://www.rfc-editor.org/rfc/rfc6749#section-6
conduitRefreshTokenRequest ::
  (MonadIO m, HasRefreshTokenRequest a) =>
  IdpApplication i a ->
  Manager ->
  OAuth2.RefreshToken ->
  ExceptT TokenResponseError m OAuth2Token
conduitRefreshTokenRequest IdpApplication {..} mgr rt =
  let tokenReq = mkRefreshTokenRequestParam application rt
      body = unionMapsToQueryParams [toQueryParam tokenReq]
   in doJSONPostRequest mgr (mkOAuth2Key application) (idpTokenEndpoint idp) body
