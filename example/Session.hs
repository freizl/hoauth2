{- mimic server side session store -}

module Session where

import           Control.Concurrent.MVar
import qualified Data.HashMap.Strict     as Map
import           Data.Maybe
import           Data.Text.Lazy          (Text)

import           Types

type KeyCache = MVar (Map.HashMap IDP IDPData)

initKeyCache :: IO KeyCache
initKeyCache = newMVar Map.empty

allValues :: KeyCache -> IO [IDPData]
allValues store = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeKey :: KeyCache -> IDP -> IO ()
removeKey store idpKey = do
  m1 <- takeMVar store
  let m2 = Map.update updateIdpData idpKey m1
  putMVar store m2
  where updateIdpData idpD = Just $ idpD { loginUser = Nothing }

lookupKey :: KeyCache
          -> IDP
          -> IO (Maybe IDPData)
lookupKey store key = do
  m1 <- tryReadMVar store
  return $ maybe Nothing (Map.lookup key) m1

insertKeys :: KeyCache -> IDP -> IDPData -> IO ()
insertKeys store key val = do
  m1 <- takeMVar store
  let m2 = Map.insert key val m1
  putMVar store m2
