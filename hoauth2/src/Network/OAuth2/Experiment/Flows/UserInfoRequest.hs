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

-- | Standard approach of fetching /userinfo
conduitUserInfoRequest ::
  (HasUserInfoRequest a, FromJSON b, MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
conduitUserInfoRequest = conduitUserInfoRequestWithCustomMethod authGetJSON

-- | Usually 'conduitUserInfoRequest' is good enough.
-- But some IdP has different approach to fetch user information rather than GET.
-- This method gives the flexiblity.
conduitUserInfoRequestWithCustomMethod ::
  (HasUserInfoRequest a, FromJSON b, MonadIO m) =>
  ( Manager ->
    AccessToken ->
    URI ->
    ExceptT BSL.ByteString m b
  ) ->
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
conduitUserInfoRequestWithCustomMethod fetchMethod IdpApplication {..} mgr at =
  fetchMethod mgr at (idpUserInfoEndpoint idp)
