{- mimic server side session store -}

module Session where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import Data.HashMap.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Idp
import Network.OAuth2.Experiment
import Types

newtype AuthorizationGrantUserStore = AuthorizationGrantUserStore (MVar (Map.HashMap TL.Text IdpAuthorizationCodeAppSessionData))

-- For the sake of simplicity for this demo App,
-- I store user data in MVar in server side.
-- It means user session shared across browsers.
-- which simplify my testing cross browsers.
-- I am sure you don't want to this for your production services.
initUserStore ::
  IO AuthorizationGrantUserStore
initUserStore = do
  let allIdps = fmap (\idpName -> (idpName, def {idpName = idpName})) supportedIdps
  AuthorizationGrantUserStore <$> newMVar (Map.fromList allIdps)

insertCodeVerifier ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  Maybe CodeVerifier ->
  ExceptT TL.Text IO ()
insertCodeVerifier store idpName val = do
  sdata <- lookupAppSessionData store idpName
  let newData = sdata {authorizePkceCodeVerifier = val}
  liftIO $ upsertAppSessionData store idpName newData

upsertAppSessionData ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  -- | idpName
  IdpAuthorizationCodeAppSessionData ->
  IO ()
upsertAppSessionData (AuthorizationGrantUserStore store) idpName val = do
  m1 <- takeMVar store
  let m2 =
        if Map.member idpName m1
          then Map.adjust (const val) idpName m1
          else Map.insert idpName val m1
  putMVar store m2

allAppSessionData :: AuthorizationGrantUserStore -> IO [IdpAuthorizationCodeAppSessionData]
allAppSessionData (AuthorizationGrantUserStore store) = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeAppSessionData ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  IO ()
removeAppSessionData store idpName = do
  upsertAppSessionData store idpName (def {idpName = idpName})

lookupAppSessionData ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  ExceptT TL.Text IO IdpAuthorizationCodeAppSessionData
lookupAppSessionData (AuthorizationGrantUserStore store) idpName = do
  mm <- liftIO $ tryReadMVar store
  m1 <-
    except $
      maybe
        (Left "[lookupAppSessionData] store (mvar) is empty")
        Right
        mm
  except $
    maybe
      (Left $ "[lookupAppSessionData] unable to find cache data for idp " <> idpName)
      Right
      (Map.lookup idpName m1)
