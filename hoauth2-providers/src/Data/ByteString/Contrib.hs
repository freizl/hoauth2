{-# LANGUAGE CPP #-}

module Data.ByteString.Contrib where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL

bsToStrict :: BSL.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0,11,0)
bsToStrict = BS.toStrict
#else
bsToStrict = BSL.toStrict
#endif

bsFromStrict :: BS.ByteString -> BSL.ByteString
#if MIN_VERSION_bytestring(0,11,0)
bsFromStrict = BS.fromStrict
#else
bsFromStrict = BSL.fromStrict
#endif
