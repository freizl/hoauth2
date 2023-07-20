module AppEnv where

import Data.HashMap.Strict (HashMap)
import Env
import Idp
import Session
import Types

-------------------------------------------------------------------------------
--                                  App Env                                  --
-------------------------------------------------------------------------------
data AppEnv = AppEnv
  { oauthAppSettings :: OAuthAppSettings
  , allIdps :: HashMap IdpName DemoIdp
  , sessionData :: AuthorizationGrantUserStore
  }
