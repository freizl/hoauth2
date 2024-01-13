module Network.OAuth2.Experiment.Pkce (
  mkPkceParam,
  CodeChallenge (..),
  CodeVerifier (..),
  CodeChallengeMethod (..),
  PkceRequestParam (..),
) where

import Control.Monad.IO.Class
import Crypto.Hash qualified as H
import Crypto.Random qualified as Crypto
import Data.Base64.Types qualified as B64
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word

newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}

newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text}

data CodeChallengeMethod = S256
  deriving (Show)

data PkceRequestParam = PkceRequestParam
  { codeVerifier :: CodeVerifier
  , codeChallenge :: CodeChallenge
  , codeChallengeMethod :: CodeChallengeMethod
  -- ^ spec says optional but in practice it is S256
  -- https://datatracker.ietf.org/doc/html/rfc7636#section-4.3
  }

mkPkceParam :: MonadIO m => m PkceRequestParam
mkPkceParam = do
  codeV <- genCodeVerifier
  pure
    PkceRequestParam
      { codeVerifier = CodeVerifier (T.decodeUtf8 codeV)
      , codeChallenge = CodeChallenge (encodeCodeVerifier codeV)
      , codeChallengeMethod = S256
      }

encodeCodeVerifier :: BS.ByteString -> Text
encodeCodeVerifier = B64.extractBase64 . B64.encodeBase64Unpadded . BS.pack . ByteArray.unpack . hashSHA256

genCodeVerifier :: MonadIO m => m BS.ByteString
genCodeVerifier = liftIO $ getBytesInternal BS.empty

cvMaxLen :: Int
cvMaxLen = 128

-- The default 'getRandomBytes' generates bytes out of unreverved characters scope.
-- code-verifier = 43*128unreserved
--   unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
--   ALPHA = %x41-5A / %x61-7A
--   DIGIT = %x30-39
getBytesInternal :: BS.ByteString -> IO BS.ByteString
getBytesInternal ba
  | BS.length ba >= cvMaxLen = pure (BS.take cvMaxLen ba)
  | otherwise = do
      bs <- Crypto.getRandomBytes cvMaxLen
      let bsUnreserved = ba `BS.append` BS.filter isUnreversed bs
      getBytesInternal bsUnreserved

hashSHA256 :: BS.ByteString -> H.Digest H.SHA256
hashSHA256 = H.hash

isUnreversed :: Word8 -> Bool
isUnreversed w = w `BS.elem` unreverseBS

{-
a-z: 97-122
A-Z: 65-90
-: 45
.: 46
_: 95
~: 126
-}
unreverseBS :: BS.ByteString
unreverseBS = BS.pack $ [97 .. 122] ++ [65 .. 90] ++ [45, 46, 95, 126]
