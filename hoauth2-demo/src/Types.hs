{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Aeson
import Data.Hashable
import Data.String
import Data.Text.Lazy (Text)
import Network.OAuth2.Experiment
import Text.Mustache qualified as M
import User

data DemoIdp
  = forall i.
    ( HasDemoLoginUser i
    , FromJSON (IdpUserInfo i)
    ) =>
    DemoIdp (Idp i)

instance Eq DemoIdp where
  -- endpoint equality is good enough to deduce they are same Idp i
  (==) :: DemoIdp -> DemoIdp -> Bool
  (DemoIdp a) == (DemoIdp b) = idpAuthorizeEndpoint a == idpAuthorizeEndpoint b

newtype IdpName = IdpName Text
  deriving (IsString)
  deriving newtype (Hashable, Ord, Eq)

instance M.ToMustache IdpName where
  toMustache (IdpName n) = M.toMustache n
