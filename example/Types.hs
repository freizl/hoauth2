{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where

import           Control.Concurrent.MVar
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8        as BSL
import           Data.Hashable
import qualified Data.HashMap.Strict               as Map
import           Data.Maybe
import           Data.Text.Lazy
import qualified Data.Text.Lazy                    as TL
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Text.Mustache
import qualified Text.Mustache                     as M

type IDPLabel = Text

-- TODO: how to make following type work??
-- type CacheStore = forall a. IDP a => MVar (Map.HashMap a IDPData)
type CacheStore = MVar (Map.HashMap IDPLabel IDPData)

-- * type class for defining a IDP
--
class (Hashable a, Show a) => IDP a

class (IDP a) => HasLabel a where
  idpLabel :: a -> IDPLabel
  idpLabel = TL.pack . show

class (IDP a) => HasAuthUri a where
  authUri :: a -> Text

class (IDP a) => HasTokenReq a where
  tokenReq :: a -> Manager -> ExchangeToken -> IO (OAuth2Result TR.Errors OAuth2Token)

class (IDP a) => HasTokenRefreshReq a where
  tokenRefreshReq :: a -> Manager -> RefreshToken -> IO (OAuth2Result TR.Errors OAuth2Token)

class (IDP a) => HasUserReq a where
  userReq :: a -> Manager -> AccessToken -> IO (Either BSL.ByteString LoginUser)

-- Heterogenous collections
-- https://wiki.haskell.org/Heterogenous_collections
--
data IDPApp = forall a. (IDP a,
                         HasTokenRefreshReq a,
                         HasTokenReq a,
                         HasUserReq a,
                         HasLabel a,
                         HasAuthUri a) => IDPApp a

-- dummy oauth2 request error
--
data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

newtype LoginUser =
  LoginUser { loginUserName :: Text
            } deriving (Eq, Show)

data IDPData =
  IDPData { codeFlowUri     :: Text
          , loginUser       :: Maybe LoginUser
          , oauth2Token     :: Maybe OAuth2Token
          , idpDisplayLabel :: IDPLabel
          }

-- simplify use case to only allow one idp instance for now.
instance Eq IDPData where
  a == b = idpDisplayLabel a == idpDisplayLabel b

instance Ord IDPData where
  a `compare` b = idpDisplayLabel a `compare` idpDisplayLabel b

newtype TemplateData = TemplateData { idpTemplateData :: [IDPData]
                                    } deriving (Eq)

-- * Mustache instances
instance ToMustache IDPData where
  toMustache t' = M.object
    [ "codeFlowUri" ~> codeFlowUri t'
    , "isLogin" ~> isJust (loginUser t')
    , "user" ~> loginUser t'
    , "name" ~> TL.unpack (idpDisplayLabel t')
    ]

instance ToMustache LoginUser where
  toMustache t' = M.object
    [ "name" ~> loginUserName t' ]

instance ToMustache TemplateData where
  toMustache td' = M.object
    [ "idps" ~> idpTemplateData td'
    ]
