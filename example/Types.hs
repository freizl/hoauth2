{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Text.Mustache
import qualified Text.Mustache    as M
import           Network.OAuth.OAuth2
import           Data.Text.Lazy
import qualified Data.Text.Lazy as TL
import Data.Maybe

data IDP = Okta | Github | Google
  deriving (Show, Eq)

idpFromText :: Text -> Maybe IDP
idpFromText ift = case (TL.unpack $ TL.toLower ift) of
  "okta" -> Just Okta
  "github" -> Just Github
  "google" -> Just Google
  _ -> Nothing

data LoginUser =
  LoginUser { loginUserName :: Text
            } deriving (Eq)

data IDPData = IDPData
  { codeFlowUri :: Text
  , loginUser :: Maybe LoginUser
  , idpName :: IDP
  , oauth2Key :: OAuth2
  } deriving (Eq)

-- TODO: make type family
mkIDPData :: IDP -> OAuth2 -> Text -> IDPData
mkIDPData idp key uri =
    IDPData { codeFlowUri = uri
            , loginUser = Nothing
            , idpName = idp
            , oauth2Key = key
            }
data TemplateData =
  TemplateData { idpData :: [IDPData]
               } deriving (Eq)


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
