{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.UserInfoRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment.Types
import URI.ByteString (URI)

-------------------------------------------------------------------------------
--                             User Info Request                             --
-------------------------------------------------------------------------------

class HasUserInfoRequest a

conduitUserInfoRequest ::
  (HasUserInfoRequest a, FromJSON b, MonadIO m) =>
  ( Manager ->
    AccessToken ->
    URI ->
    ExceptT BSL.ByteString m b
  ) ->
  -- | The way to fetch userinfo. IdP may use different approach rather than just GET.
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
conduitUserInfoRequest fetchMethod IdpApplication {..} mgr at =
  fetchMethod mgr at (idpUserInfoEndpoint idp)
