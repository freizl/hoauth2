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
-- import GHC.Generics
import Network.OAuth2.Experiment
import Text.Mustache qualified as M
import User

data DemoIdp
  = forall i.
    ( HasDemoLoginUser i
    , FromJSON (IdpUserInfo i)
    ) =>
    DemoIdp (Idp i)

-- data IdpName
--   = Auth0
--   | AzureAD
--   | DropBox
--   | Facebook
--   | Fitbit
--   | GitHub
--   | Google
--   | LinkedIn
--   | Okta
--   | Slack
--   | StackExchange
--   | Twitter
--   | Weibo
--   | ZOHO
-- deriving (Eq, Ord, Show, Generic, Hashable, Read)

deriving instance Hashable IdpName

deriving instance Read IdpName

toText :: IdpName -> Text
toText = TL.pack . show

-- Hack but good enough for demo app
fromText :: Text -> IdpName
fromText = read . TL.unpack

instance M.ToMustache IdpName where
  toMustache idpName = M.toMustache (show idpName)
