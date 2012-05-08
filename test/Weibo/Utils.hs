
module Utils where

import qualified Text.Show.ByteString as TSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

intToByteString :: Int -> BS.ByteString
intToByteString = toStrickBS' . TSB.show

toStrickBS' :: LBS.ByteString -> BS.ByteString
toStrickBS' = BS.concat . LBS.toChunks