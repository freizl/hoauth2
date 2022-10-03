{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth2.Experiment
import Text.Mustache
import Text.Mustache qualified as M
import Prelude hiding (id)

class HasDemoLoginUser a where
  toLoginUser :: IdpUserInfo a -> DemoLoginUser

-- | Use for creating list of IDPs
-- Heterogenous collections
-- https://wiki.haskell.org/Heterogenous_collections
data DemoAuthorizationApp
  = forall a b.
    ( HasDemoLoginUser b,
      FromJSON (IdpUserInfo b),
      'AuthorizationCode ~ a,
      HasPkceAuthorizeRequest a,
      HasPkceTokenRequest a,
      HasUserInfoRequest a,
      HasIdpAppName a,
      HasAuthorizeRequest a,
      HasTokenRequest a,
      HasRefreshTokenRequest a
    ) =>
    DemoAuthorizationApp (IdpApplication a b)

newtype DemoLoginUser = DemoLoginUser
  { loginUserName :: TL.Text
  -- TODO: maybe email
  }
  deriving (Eq, Show)

data DemoAppPerAppSessionData = DemoAppPerAppSessionData
  { loginUser :: Maybe DemoLoginUser,
    oauth2Token :: Maybe OAuth2Token,
    authorizePkceCodeVerifier :: Maybe CodeVerifier,
    authorizeAbsUri :: TL.Text
  }

data DemoAppEnv = DemoAppEnv DemoAuthorizationApp DemoAppPerAppSessionData

instance Default DemoAppPerAppSessionData where
  def =
    DemoAppPerAppSessionData
      { loginUser = Nothing,
        oauth2Token = Nothing,
        authorizePkceCodeVerifier = Nothing,
        authorizeAbsUri = ""
      }

instance Show DemoAppEnv where
  show :: DemoAppEnv -> String
  show = TL.unpack . toLabel

toLabel :: DemoAppEnv -> TL.Text
toLabel (DemoAppEnv (DemoAuthorizationApp idpAppConfig) _) = getIdpAppName idpAppConfig

-- simplify use case to only allow one idp instance for now.
instance Eq DemoAppEnv where
  a == b = toLabel a == toLabel b

instance Ord DemoAppEnv where
  a `compare` b = toLabel a `compare` toLabel b

newtype TemplateData = TemplateData
  { idpTemplateData :: [DemoAppEnv]
  }
  deriving (Eq)

-- * Mustache instances

instance ToMustache DemoAppEnv where
  toMustache (DemoAppEnv (DemoAuthorizationApp idpAppConfig) DemoAppPerAppSessionData {..}) =
    M.object
      [ "codeFlowUri" ~> authorizeAbsUri,
        "isLogin" ~> isJust loginUser,
        "user" ~> loginUser,
        "name" ~> TL.unpack (getIdpAppName idpAppConfig)
      ]

instance ToMustache DemoLoginUser where
  toMustache t' =
    M.object
      ["name" ~> loginUserName t']

instance ToMustache TemplateData where
  toMustache td' =
    M.object
      [ "idps" ~> idpTemplateData td'
      ]
