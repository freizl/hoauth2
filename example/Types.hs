{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Text.Mustache
import qualified Text.Mustache    as M


data IDP = Okta | Github | Google
  deriving (Show, Eq)

data LoginUser =
  LoginUser { loginUserName :: String
            } deriving (Eq)

data IDPData = IDPData
  { codeFlowUri :: String
  , isLogin :: Bool
  , loginUser :: Maybe LoginUser
  , idpName :: IDP
  } deriving (Eq)

data TemplateData =
  TemplateData { idpData :: [IDPData]
               } deriving (Eq)


instance ToMustache IDPData where
  toMustache t' = M.object
    [ "codeFlowUri" ~> codeFlowUri t'
    , "isLogin" ~> isLogin t'
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
