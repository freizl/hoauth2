{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}

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

data IDP = Okta | Github | Google
  deriving (Show, Eq, Generic)

data Okta2 = Okta2

instance Hashable IDP

idpFromText :: Text -> Maybe IDP
idpFromText ift = case (TL.unpack $ TL.toLower ift) of
  "okta" -> Just Okta
  "github" -> Just Github
  "google" -> Just Google
  _ -> Nothing

data LoginUser =
  LoginUser { loginUserName :: Text
            } deriving (Eq, Show)

data IDPData = IDPData
  { codeFlowUri :: Text
  , loginUser :: Maybe LoginUser
  , idpName :: IDP
  , oauth2Key :: OAuth2
  } deriving (Eq, Show)

-- TODO: make type family
mkIDPData :: IDP -> OAuth2 -> Text -> IDPData
mkIDPData idp key uri = IDPData { codeFlowUri = uri
                                , loginUser = Nothing
                                , idpName = idp
                                , oauth2Key = key
                                }

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
