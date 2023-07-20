module AppEnv where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Map.Strict qualified as Map
import Data.Text.Lazy (Text)
import Env
import Idp
import Session
import Types

-------------------------------------------------------------------------------
--                                  App Env                                  --
-------------------------------------------------------------------------------
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
findIdp AppEnv {..} idpName@(IdpName name) =
  except $
    maybe
      (Left $ "Unable to lookup idp: " <> name)
      Right
      (Map.lookup idpName allIdps)
