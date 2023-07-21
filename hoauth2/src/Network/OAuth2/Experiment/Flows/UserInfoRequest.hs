{-# LANGUAGE FlexibleContexts #-}

module Network.OAuth2.Experiment.Flows.UserInfoRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment.Types

-------------------------------------------------------------------------------
--                             User Info Request                             --
-------------------------------------------------------------------------------

class HasUserInfoRequest a

conduitUserInfoRequest ::
  (HasUserInfoRequest a, FromJSON (IdpUserInfo i), MonadIO m) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m (IdpUserInfo i)
conduitUserInfoRequest IdpApplication {..} mgr at =
  authGetJSON mgr at (idpUserInfoEndpoint idp)

-- conduitUserInfoRequestWithFetchMethod ::
--   (HasUserInfoRequest a, FromJSON (IdpUserInfo i), MonadIO m) =>
--    (
--       Manager ->
--       AccessToken ->
--       URI ->
--       ExceptT BSL.ByteString m (IdpUserInfo i)
--    )
--   IdpApplication i a ->
--   Manager ->
--   AccessToken ->
--   ExceptT BSL.ByteString m (IdpUserInfo i)
-- conduitUserInfoRequestWithFetchMethod func IdpApplication {..} mgr at =
--   func mgr at (idpUserInfoEndpoint idp)
