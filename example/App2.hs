{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RankNTypes                #-}

module App2 where

import Keys
import           Data.Text.Lazy
import qualified Data.Text.Lazy as TL
import Data.Maybe
import Control.Monad
import           Data.ByteString      (ByteString)
import           Data.Hashable
import           GHC.Generics
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Data.Aeson
import qualified Data.HashMap.Strict     as Map
import           Data.Aeson.Types
import Keys

-- 1. extensible IDP
-- 2. cache
-- 3. mustache


test2 :: IO ()
test2 = putStrLn "test2"

{- tryFetchUser :: IsIdp a => a -> IO (Either Text LoginUser)
tryFetchUser idp = do
  re <- getUserInfo idp
  return re -}


newtype LoginUser =
  LoginUser { loginUserName :: Text
            } deriving (Eq, Show)

data Method = GET | POST
type Param = (ByteString, ByteString)

data Req =
  Req { body :: [Param]
      , method :: Method
      , query :: [Param]
      }

data G = G deriving (Show, Generic)
data GithubUser = GithubUser deriving (Show, Generic)

data FB = FB deriving (Show, Generic)
data FacebookUser = FacebookUser deriving (Show, Generic)

instance Hashable G
instance Hashable FB

instance FromJSON FacebookUser where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON GithubUser where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

type KeyCache = forall a. OAuthIDP a => (Map.HashMap a IDPData)


data IDPData =
  IDPData { codeFlowReq :: Req
          , loginUser   :: Maybe LoginUser
          , idpName     :: forall i. OAuthIDP i => i
          , oauth2Config   :: OAuth2
          , accessTokenReq :: Req
          , userInfoReq :: Req
          , toLoginUser :: forall b. FromJSON b => b -> LoginUser
          }

class OAuthIDP i
instance OAuthIDP G
instance OAuthIDP FB

allIDPs :: [IDPData]
allIDPs =
  [ IDPData { codeFlowReq = Req [] GET []
            , loginUser = Nothing
            , idpName = G
            , oauth2Config = githubKey
            , accessTokenReq = Req [] GET []
            , userInfoReq = Req [] GET []
            , toLoginUser = githubUserToLoginUser
            }
  ]

githubUserToLoginUser :: GithubUser -> LoginUser
githubUserToLoginUser = undefined

facebookUserToLoginUser :: FacebookUser -> LoginUser
facebookUserToLoginUser = undefined
