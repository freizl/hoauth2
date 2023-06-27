{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- mimic server side session store -}

module Session where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.HashMap.Strict qualified as Map
import Data.Text.Lazy qualified as TL
import Types

type CacheStore = MVar (Map.HashMap TL.Text DemoAppEnv)

initCacheStore :: IO CacheStore
initCacheStore = newMVar Map.empty

allValues :: CacheStore -> IO [DemoAppEnv]
allValues store = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeKey :: CacheStore -> TL.Text -> IO ()
removeKey store idpKey = do
  m1 <- takeMVar store
  let m2 = Map.update updateIdpData idpKey m1
  putMVar store m2
  where
    updateIdpData (DemoAppEnv app sessionD) = Just (DemoAppEnv app sessionD {loginUser = Nothing})

lookupKey ::
  MonadIO m =>
  CacheStore ->
  TL.Text ->
  ExceptT TL.Text m DemoAppEnv
lookupKey store idpKey = ExceptT $ do
  m1 <- liftIO $ tryReadMVar store
  return $ maybe (Left ("unknown Idp " <> idpKey)) Right (Map.lookup idpKey =<< m1)

upsertDemoAppEnv :: MonadIO m => CacheStore -> DemoAppEnv -> ExceptT TL.Text m ()
upsertDemoAppEnv store val = liftIO $ do
  m1 <- takeMVar store
  let m2 =
        if Map.member (toLabel val) m1
          then Map.adjust (const val) (toLabel val) m1
          else Map.insert (toLabel val) val m1
  putMVar store m2
