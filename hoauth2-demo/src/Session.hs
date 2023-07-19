{- mimic server side session store -}

module Session where

import Data.Default
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.HashMap.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Network.OAuth2.Experiment
import Types

type AuthorizationGrantUserStore = MVar (Map.HashMap TL.Text DemoAppPerAppSessionData)

initUserStore ::
  IO AuthorizationGrantUserStore
initUserStore = do
  let allIdps = fmap (\idpName -> (idpName, def {idpName = idpName} )) supportedIdps
  newMVar $ Map.fromList allIdps

insertCodeVerifier ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  Maybe CodeVerifier ->
  ExceptT TL.Text IO ()
insertCodeVerifier store idpName val = do
  sdata <- lookupKey2 store idpName
  let newData = sdata {authorizePkceCodeVerifier = val}
  liftIO $ upsertDemoUserData store idpName newData

removeKey2 ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  IO ()
removeKey2 store idpName = do
  upsertDemoUserData store idpName (def {idpName = idpName})

lookupKey2 ::
  AuthorizationGrantUserStore ->
  TL.Text ->
  ExceptT TL.Text IO DemoAppPerAppSessionData
lookupKey2 store idpName = do
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

type CacheStore = MVar (Map.HashMap TL.Text DemoAppEnv)

initCacheStore :: IO CacheStore
initCacheStore = newMVar Map.empty


removeKey :: CacheStore -> TL.Text -> IO ()
removeKey store idpKey = do
  m1 <- takeMVar store
  let m2 = Map.update updateIdpData idpKey m1
  putMVar store m2
  where
    updateIdpData (DemoAppEnv app sessionD) = Just (DemoAppEnv app sessionD {loginUser = Nothing})

lookupKey ::
  (MonadIO m) =>
  CacheStore ->
  TL.Text ->
  ExceptT TL.Text m DemoAppEnv
lookupKey store idpKey = ExceptT $ do
  m1 <- liftIO $ tryReadMVar store
  return $ maybe (Left ("unknown Idp " <> idpKey)) Right (Map.lookup idpKey =<< m1)

upsertDemoAppEnv :: (MonadIO m) => CacheStore -> DemoAppEnv -> ExceptT TL.Text m ()
upsertDemoAppEnv store val = liftIO $ do
  m1 <- takeMVar store
  let m2 =
        if Map.member (toLabel val) m1
          then Map.adjust (const val) (toLabel val) m1
          else Map.insert (toLabel val) val m1
  putMVar store m2
