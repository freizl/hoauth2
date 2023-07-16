module Network.HTTP.Client.Contrib where

import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BSL
import Network.HTTP.Conduit
import Network.HTTP.Types qualified as HT

-- | Get response body out of a @Response@
handleResponse :: Response BSL.ByteString -> Either BSL.ByteString BSL.ByteString
handleResponse rsp
  | HT.statusIsSuccessful (responseStatus rsp) = Right (responseBody rsp)
  -- FIXME: better to surface up entire resp so that client can decide what to do when error happens.
  -- e.g. when 404, the response body could be empty hence library user has no idea what's happening.
  -- Which will be breaking changes.
  -- The current work around is surface up entire response as string.
  | BSL.null (responseBody rsp) = Left (BSL.pack $ show rsp)
  | otherwise = Left (responseBody rsp)

handleResponseJSON ::
  FromJSON a =>
  Response BSL.ByteString ->
  Either BSL.ByteString a
handleResponseJSON =
  either Left (first BSL.pack . eitherDecode) . handleResponse
