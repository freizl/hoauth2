{-|
Module      : Network.HTTP.Client.Contrib
Description : HTTP Client utilities for OAuth2 requests
Copyright   : (c) Haisheng Wu
License     : MIT
Maintainer  : freizl@gmail.com
Stability   : stable

This module provides helper functions for handling HTTP responses in OAuth2 context,
with special focus on error handling and JSON response processing.
-}
module Network.HTTP.Client.Contrib where

import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BSL
import Network.HTTP.Conduit
import Network.HTTP.Types qualified as HT

-- | Extract and validate response body from an HTTP 'Response'.
-- 
-- For successful responses (2xx status codes), returns the response body.
-- For error responses, returns either:
--   * The response body if it's not empty (typically contains error details)
--   * A string representation of the entire response if body is empty
-- 
-- Example:
-- 
-- @
-- case handleResponse response of
--   Right body -> processSuccessResponse body
--   Left err   -> handleErrorResponse err
-- @
handleResponse :: Response BSL.ByteString -> Either BSL.ByteString BSL.ByteString
handleResponse rsp
  | HT.statusIsSuccessful (responseStatus rsp) = Right (responseBody rsp)
  -- TODO: better to surface up entire resp so that client can decide what to do when error happens.
  -- e.g. when 404, the response body could be empty hence library user has no idea what's happening.
  -- Which will be breaking changes.
  -- The current work around is surface up entire response as string.
  | BSL.null (responseBody rsp) = Left (BSL.pack $ show rsp)
  | otherwise = Left (responseBody rsp)

-- | Process HTTP response and attempt to parse its body as JSON.
-- 
-- This is a convenience function that combines 'handleResponse' with JSON parsing.
-- For successful responses, attempts to parse the body as JSON.
-- For error responses, returns the error message as is.
-- 
-- Example:
-- 
-- @
-- case handleResponseJSON response of
--   Right userInfo -> processUser userInfo
--   Left err       -> handleError err
-- @
--
-- Note: JSON parsing errors are returned as text in the Left value.
handleResponseJSON ::
  FromJSON a =>
  Response BSL.ByteString ->
  Either BSL.ByteString a
handleResponseJSON =
  either Left (first BSL.pack . eitherDecode) . handleResponse
