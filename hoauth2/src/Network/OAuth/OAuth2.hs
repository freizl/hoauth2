------------------------------------------------------------

------------------------------------------------------------

-- |
-- Module      :  Network.OAuth.OAuth2
-- Description :  OAuth2 client
-- Copyright   :  (c) 2012 Haisheng Wu
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Haisheng Wu <freizl@gmail.com>
-- Stability   :  Beta
-- Portability :  portable
--
-- A lightweight oauth2 haskell binding.
module Network.OAuth.OAuth2
  ( module Network.OAuth.OAuth2.HttpClient,
    module Network.OAuth.OAuth2.AuthorizationRequest,
    module Network.OAuth.OAuth2.TokenRequest,
    module Network.OAuth.OAuth2.Internal,
  )
where

{-
  Hiding Errors data type from default.
  Shall qualified import given the naming conflicts.
-}
import Network.OAuth.OAuth2.AuthorizationRequest hiding (Errors(..))
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2.Internal
import Network.OAuth.OAuth2.TokenRequest hiding (Errors(..))
