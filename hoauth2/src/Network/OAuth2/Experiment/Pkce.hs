module Network.OAuth2.Experiment.Pkce (
  mkPkceParam,
  CodeChallenge (..),
  CodeVerifier (..),
  CodeChallengeMethod (..),
  PkceRequestParam (..),
) where

import Data.List (unfoldr)
import Control.Monad.IO.Class (MonadIO(..))
import Crypto.Hash.SHA256 qualified as H
import qualified System.Random.SplitMix as SM (SMGen, nextWord32, initSMGen)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word

newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}

newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text} deriving (Show)

data CodeChallengeMethod = S256
  deriving (Show)

data PkceRequestParam = PkceRequestParam
  { codeVerifier :: CodeVerifier
  , codeChallenge :: CodeChallenge
  , codeChallengeMethod :: CodeChallengeMethod
  -- ^ spec says optional but really it shall be s256 or can be omitted?
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
encodeCodeVerifier = B64.encodeBase64Unpadded . H.hash

genCodeVerifier :: MonadIO m => m BS.ByteString
genCodeVerifier = liftIO $ getRandomBytes cvMaxLen

cvMaxLen :: Int
cvMaxLen = 128

-- The default 'getRandomBytes' generates bytes out of unreserved characters scope.
-- code-verifier = 43*128unreserved
--   unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
--   ALPHA = %x41-5A / %x61-7A
--   DIGIT = %x30-39
getRandomBytes :: Int -> IO BS.ByteString
getRandomBytes l = do
  s0 <- SM.initSMGen
  pure $ unreservedBytes l s0

unreservedBytes :: Int -> SM.SMGen -> BS.ByteString
unreservedBytes maxBytes s0 = BS.pack $ unfoldr mk (0, s0)
  where
    mk (i, s)
      | i == maxBytes = Nothing
      | otherwise =
          let
            (w32, s') = SM.nextWord32 s
            w8 = (fromIntegral w32 :: Word8) -- we're wasting 24 out of 32 bits here
          in
            if w8 `elem` unreserveBS
            then Just (w8, (i + 1, s')) -- accept byte
            else mk (i, s') -- loop

{-
a-z: 97-122
A-Z: 65-90
-: 45
.: 46
_: 95
~: 126
-}
unreserveBS :: [Word8]
unreserveBS = [97 .. 122] ++ [65 .. 90] ++ [45, 46, 95, 126]



