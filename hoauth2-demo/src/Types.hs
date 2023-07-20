{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Hashable
import Data.String
import Data.Text.Lazy (Text)
import Text.Mustache qualified as M

newtype IdpName = IdpName Text
  deriving (IsString)
  deriving newtype (Hashable, Ord, Eq)

instance M.ToMustache IdpName where
  toMustache (IdpName n) = M.toMustache n
