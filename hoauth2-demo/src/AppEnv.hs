module AppEnv where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Text.Lazy (Text)
import Env
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Session
import Types

-------------------------------------------------------------------------------
--                                  App Env                                  --
-------------------------------------------------------------------------------
type TenantBasedIdps = (Idp Auth0, Idp Okta)

data AppEnv = AppEnv
  { oauthAppSettings :: OAuthAppSettings
  , allIdps :: Map.Map IdpName DemoIdp
  , sessionStore :: AuthorizationGrantUserStore
  }

getIdpNames :: Map.Map IdpName DemoIdp -> [IdpName]
getIdpNames = Map.keys

findIdp ::
  MonadIO m =>
  AppEnv ->
  IdpName ->
  ExceptT Text m DemoIdp
findIdp AppEnv {..} idpName =
  except $
    maybe
      (Left $ "Unable to lookup idp: " <> toText idpName)
      Right
      (Map.lookup idpName allIdps)
