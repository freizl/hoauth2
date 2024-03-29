{- mimic server side session store -}

module Session where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Types

-- For the sake of simplicity for this demo App,
-- I store user data in MVar in server side.
-- It means user session shared across browsers.
-- which simplify my testing cross browsers.
-- I am sure you don't want to this for your production services.
initUserStore ::
  [IdpName] ->
  IO AuthorizationGrantUserStore
initUserStore = do
  fmap AuthorizationGrantUserStore
    . newMVar
    . Map.fromList
    . fmap (\idpName -> (idpName, def {idpName = idpName}))

insertCodeVerifier ::
  AuthorizationGrantUserStore ->
  IdpName ->
  Maybe CodeVerifier ->
  ExceptT TL.Text IO ()
insertCodeVerifier store idpName val = do
  sdata <- lookupAppSessionData store idpName
  let newData = sdata {authorizePkceCodeVerifier = val}
  liftIO $ upsertAppSessionData store idpName newData

upsertAppSessionData ::
  AuthorizationGrantUserStore ->
  IdpName ->
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
  IdpName ->
  IO ()
removeAppSessionData store idpName = do
  upsertAppSessionData store idpName (def {idpName = idpName})

lookupAppSessionData ::
  AuthorizationGrantUserStore ->
  IdpName ->
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
      (Left $ "[lookupAppSessionData] unable to find cache data for idp " <> toText idpName)
      Right
      (Map.lookup idpName m1)
