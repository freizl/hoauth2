{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Monad.Trans.Except
import Control.Concurrent.MVar
import Data.Aeson
import Data.Aeson.KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Lazy
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import Text.Mustache
import qualified Text.Mustache as M

type IDPLabel = Text

type CacheStore = MVar (Map.HashMap IDPLabel IDPData)

-- TODO: how to allow override domain (auth0, okta) ?
--
data EnvConfigCreds = EnvConfigCreds
  { clientId :: T.Text,
    clientSecret :: T.Text
  }
  deriving (Generic)

instance FromJSON EnvConfigCreds

type EnvConfig = KeyMap EnvConfigCreds

-- * type class for defining a IDP

class (Hashable a, Show a) => IDP a

class HasLabel a where
  idpLabel :: a -> IDPLabel

-- TODO: the `a` in following classes are not actually not used as value
-- but more of a indicator. Wonder if type level programming
-- or other solution would simplify the logic.?
--
class (IDP a) => HasAuthUri a where
  authUri :: a -> Text

-- TODO: consider to convert to ExceptT?
class (IDP a) => HasTokenReq a where
  tokenReq :: a -> Manager -> ExchangeToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token

class (IDP a) => HasTokenRefreshReq a where
  tokenRefreshReq :: a -> Manager -> RefreshToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token

-- | TODO: associates userInfo uri and toLoginUser method
-- so that can have default implementation for userReq
--
class (IDP a) => HasUserReq a where
  userReq :: a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO LoginUser

-- Heterogenous collections
-- https://wiki.haskell.org/Heterogenous_collections
--
data IDPApp
  = forall a.
    ( HasTokenRefreshReq a,
      HasTokenReq a,
      HasUserReq a,
      HasLabel a,
      HasAuthUri a
    ) =>
    IDPApp a

-- dummy oauth2 request error
--
data Errors
  = SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True}

newtype LoginUser = LoginUser
  { loginUserName :: Text
  }
  deriving (Eq, Show)

data IDPData = IDPData
  { idpApp :: IDPApp,
    loginUser :: Maybe LoginUser,
    oauth2Token :: Maybe OAuth2Token
  }

instance Show IDPData where
  show = TL.unpack . toLabel

toLabel :: IDPData -> IDPLabel
toLabel (IDPData (IDPApp a) _ _) = idpLabel a

-- simplify use case to only allow one idp instance for now.
instance Eq IDPData where
  a == b = toLabel a == toLabel b

instance Ord IDPData where
  a `compare` b = toLabel a `compare` toLabel b

newtype TemplateData = TemplateData
  { idpTemplateData :: [IDPData]
  }
  deriving (Eq)

-- * Mustache instances

instance ToMustache IDPData where
  toMustache (IDPData (IDPApp a) lu _) =
    M.object
      [ "codeFlowUri" ~> authUri a,
        "isLogin" ~> isJust lu,
        "user" ~> lu,
        "name" ~> TL.unpack (idpLabel a)
      ]

instance ToMustache LoginUser where
  toMustache t' =
    M.object
      ["name" ~> loginUserName t']

instance ToMustache TemplateData where
  toMustache td' =
    M.object
      [ "idps" ~> idpTemplateData td'
      ]
