{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.RefreshTokenRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Utils

-------------------------------------------------------------------------------
--                            RefreshToken Requset                           --
-------------------------------------------------------------------------------

-- | https://www.rfc-editor.org/rfc/rfc6749#page-47
class (HasOAuth2Key a, HasTokenRequestClientAuthenticationMethod a) => HasRefreshTokenRequest a where
  data RefreshTokenRequest a
  mkRefreshTokenRequestParam :: a -> OAuth2.RefreshToken -> RefreshTokenRequest a

conduitRefreshTokenRequest ::
  (HasRefreshTokenRequest a, ToQueryParam (RefreshTokenRequest a), MonadIO m) =>
  IdpApplication a i ->
  Manager ->
  OAuth2.RefreshToken ->
  ExceptT TokenRequestError m OAuth2Token
conduitRefreshTokenRequest IdpApplication {..} mgr rt =
  let tokenReq = mkRefreshTokenRequestParam application rt
      body = mapsToParams [toQueryParam tokenReq]
   in doJSONPostRequest mgr (mkOAuth2Key application) (idpTokenEndpoint idp) body
