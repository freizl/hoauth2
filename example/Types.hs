{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Types where

import           Text.Mustache
import qualified Text.Mustache    as M
import           Network.OAuth.OAuth2
import           Data.Text.Lazy
import qualified Data.Text.Lazy as TL
import Data.Maybe
import Data.Hashable
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Network.HTTP.Conduit
import           URI.ByteString
import           URI.ByteString.QQ
import Data.Bifunctor

data IDP =
    Douban
  | Dropbox
  | Facebook
  | Fitbit
  | Github
  | Google
  | Okta
  | StackExchange
  | Weibo
  deriving (Show, Eq, Generic)

instance Hashable IDP

idpFromText :: Text -> Maybe IDP
idpFromText ift = case (TL.unpack $ TL.toLower ift) of
  "okta" -> Just Okta
  "github" -> Just Github
  "google" -> Just Google
  "douban" -> Just Douban
  "dropbox" -> Just Dropbox
  "facebook" -> Just Facebook
  "fitbit" -> Just Fitbit
  "weibo" -> Just Weibo
  "stackexchaneg" -> Just StackExchange
  _ -> Nothing

data LoginUser =
  LoginUser { loginUserName :: Text
            } deriving (Eq, Show)

data IDPData = forall a . FromJSON a =>
  IDPData { codeFlowUri :: Text
          , loginUser :: Maybe LoginUser
          , idpName :: IDP
          , oauth2Key :: OAuth2
          , userApiUri :: URI
          , toLoginUser :: a -> LoginUser
          }

-- simplify use case to only allow one idp instance for now.
instance Eq IDPData where
  a == b = (idpName a) == (idpName b)

data TemplateData = TemplateData { idpData :: [IDPData]
                                 } deriving (Eq)

-- * Mustache instances
instance ToMustache IDPData where
  toMustache t' = M.object
    [ "codeFlowUri" ~> codeFlowUri t'
    , "isLogin" ~> isJust (loginUser t')
    , "user" ~> loginUser t'
    , "name" ~> show (idpName t')
    ]

instance ToMustache LoginUser where
  toMustache t' = M.object
    [ "name" ~> loginUserName t' ]

instance ToMustache TemplateData where
  toMustache td' = M.object
    [ "idps" ~> idpData td'
    ]
