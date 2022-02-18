------------------------------------------------------------
-- |
-- Module      :  Network.OAuth.OAuth2
-- Description :  OAuth2 client
-- Copyright   :  (c) 2012 Haisheng Wu
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Haisheng Wu <freizl@gmail.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- A lightweight oauth2 haskell binding.
------------------------------------------------------------

module Network.OAuth.OAuth2
       (module Network.OAuth.OAuth2.HttpClient,
        module Network.OAuth.OAuth2.Internal
       )
       where

import           Network.OAuth.OAuth2.HttpClient
import           Network.OAuth.OAuth2.Internal
