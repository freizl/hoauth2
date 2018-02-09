{- mimic server side session store -}

module Session where

import           Control.Concurrent.MVar
import qualified Data.HashMap.Strict     as Map
import           Data.Maybe
import           Data.Text.Lazy          (Text)

type KeyCache = MVar (Map.HashMap String String)

initKeyCache :: IO KeyCache
initKeyCache = newMVar Map.empty


lookupKey :: KeyCache
          -> String
          -> IO (Maybe String)
lookupKey store key =
    Map.lookup key <$> readMVar store

insertKeys :: KeyCache -> String -> String -> IO String
insertKeys store key val = do
  m1 <- takeMVar store
  let m2 = Map.insert key value m1
  putMVar kc m2
  return val
