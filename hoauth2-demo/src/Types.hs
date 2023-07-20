{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Data.Aeson
import Data.Hashable
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Network.OAuth2.Experiment
import Text.Mustache qualified as M
import User

data DemoIdp
  = forall i.
    ( HasDemoLoginUser i
    , FromJSON (IdpUserInfo i)
    ) =>
    DemoIdp (Idp i)

deriving instance Hashable IdpName

toText :: IdpName -> Text
toText = TL.pack . show

deriving instance Read IdpName

fromText :: Text -> IdpName
fromText = read . TL.unpack

instance M.ToMustache IdpName where
  toMustache idpName = M.toMustache (show idpName)
