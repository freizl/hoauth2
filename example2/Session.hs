{-# LANGUAGE RankNTypes                #-}

{- mimic server side session store -}

module Session where

import           Control.Concurrent.MVar
import qualified Data.HashMap.Strict     as Map
import Data.Text.Lazy

import           Types

-- TODO: how to make following type work??
-- type KeyCache = forall a. IDP a => MVar (Map.HashMap a IDPData)
type KeyCache = MVar (Map.HashMap Text IDPData)

initKeyCache :: IO KeyCache
initKeyCache = newMVar Map.empty

allValues :: KeyCache -> IO [IDPData]
allValues store = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeKey :: IDPLabel a => KeyCache -> a -> IO ()
removeKey store idpKey = do
  m1 <- takeMVar store
  let m2 = Map.update updateIdpData (idpLabel idpKey) m1
  putMVar store m2
  where updateIdpData idpD = Just $ idpD { loginUser = Nothing }

lookupKey :: IDPLabel a => KeyCache
          -> a
          -> IO (Maybe IDPData)
lookupKey store key = do
  m1 <- tryReadMVar store
  return $ maybe Nothing (Map.lookup (idpLabel key)) m1

insertKeys :: KeyCache -> IDPData -> IO ()
insertKeys store val = do
  m1 <- takeMVar store
  let m2 = Map.insert (idpName val) val m1
  putMVar store m2
