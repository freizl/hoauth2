{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.RefreshTokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils

-------------------------------------------------------------------------------
--                            RefreshToken Requset                           --
-------------------------------------------------------------------------------

class (HasOAuth2Key a, HasTokenRequestClientAuthenticationMethod a) => HasRefreshTokenRequest a where
  data RefreshTokenRequest a

  -- | Make Refresh Token Request parameters
  -- | https://www.rfc-editor.org/rfc/rfc6749#section-6
  mkRefreshTokenRequestParam :: a -> OAuth2.RefreshToken -> RefreshTokenRequest a

-- | Make Refresh Token Request
-- https://www.rfc-editor.org/rfc/rfc6749#section-6
conduitRefreshTokenRequest ::
  (HasRefreshTokenRequest a, ToQueryParam (RefreshTokenRequest a), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  OAuth2.RefreshToken ->
  ExceptT TokenRequestError m OAuth2Token
conduitRefreshTokenRequest IdpApplication {..} mgr rt =
  let tokenReq = mkRefreshTokenRequestParam application rt
      body = unionMapsToQueryParams [toQueryParam tokenReq]
   in doJSONPostRequest mgr (mkOAuth2Key application) (idpTokenEndpoint idp) body
