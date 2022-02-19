{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- mimic server side session store -}

module Session where

import Control.Concurrent.MVar
import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as Map
import Data.Text
import Types

initCacheStore :: IO CacheStore
initCacheStore = newMVar Map.empty

allValues :: CacheStore -> IO [IDPData]
allValues store = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeKey :: CacheStore -> IDPLabel -> IO ()
removeKey store idpKey = do
  m1 <- takeMVar store
  let m2 = Map.update updateIdpData idpKey m1
  putMVar store m2
  where
    updateIdpData idpD = Just $ idpD {loginUser = Nothing}

lookupKey ::
  CacheStore ->
  IDPLabel ->
  ExceptT IDPLabel IO IDPData
lookupKey store idpKey = ExceptT $ do
  m1 <- tryReadMVar store
  return $ maybe (Left ("unknown IDP " <> idpKey)) Right (Map.lookup idpKey =<< m1)

upsertIDPData :: CacheStore -> IDPData -> IO ()
upsertIDPData store val = do
  m1 <- takeMVar store
  let m2 =
        if Map.member (toLabel val) m1
          then Map.adjust (const val) (toLabel val) m1
          else Map.insert (toLabel val) val m1
  putMVar store m2
