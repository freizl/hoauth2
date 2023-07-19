{- mimic server side session store -}

module Session where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import Data.HashMap.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Network.OAuth2.Experiment
import Types

type AuthorizationGrantUserStore = MVar (Map.HashMap TL.Text DemoAppPerAppSessionData)

-- For the sake of simplicity for this demo App,
-- I store user data in MVar in server side.
-- It means user session shared across browsers.
-- which simplify my testing cross browsers.
-- I am sure you don't want to this for your production services.
initUserStore ::
  IO AuthorizationGrantUserStore
initUserStore = do
  let allIdps = fmap (\idpName -> (idpName, def {idpName = idpName})) supportedIdps
  newMVar $ Map.fromList allIdps

insertCodeVerifier ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  Maybe CodeVerifier ->
  ExceptT TL.Text IO ()
insertCodeVerifier store idpName val = do
  sdata <- lookupKey store idpName
  let newData = sdata {authorizePkceCodeVerifier = val}
  liftIO $ upsertDemoUserData store idpName newData

removeKey ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  IO ()
removeKey store idpName = do
  upsertDemoUserData store idpName (def {idpName = idpName})

lookupKey ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  ExceptT TL.Text IO DemoAppPerAppSessionData
lookupKey store idpName = do
  mm <- liftIO $ tryReadMVar store
  case mm of
    Nothing -> throwE "[lookupKey] store (mvar) is empty"
    Just m1 ->
      case Map.lookup idpName m1 of
        Nothing -> throwE ("[lookupKey] unable to find cache data for idp " <> idpName)
        Just appData -> pure appData

upsertDemoUserData ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  -- | idpName
  DemoAppPerAppSessionData ->
  IO ()
upsertDemoUserData store idpName val = do
  m1 <- takeMVar store
  let m2 =
        if Map.member idpName m1
          then Map.adjust (const val) idpName m1
          else Map.insert idpName val m1
  putMVar store m2

allValues :: AuthorizationGrantUserStore -> IO [DemoAppPerAppSessionData]
allValues store = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1
