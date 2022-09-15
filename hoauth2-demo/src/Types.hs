{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Control.Concurrent.MVar
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import Text.Mustache
import qualified Text.Mustache as M
import URI.ByteString
import URI.ByteString.QQ
import Utils

type IDPLabel = TL.Text

type CacheStore = MVar (Map.HashMap IDPLabel IDPData)

-- TODO: how to allow override domain (auth0, okta) ?
--
data EnvConfigAuthParams = EnvConfigAuthParams
  { clientId :: T.Text,
    clientSecret :: T.Text,
    scopes :: Maybe [T.Text]
  }
  deriving (Generic)

instance Default EnvConfigAuthParams where
  def =
    EnvConfigAuthParams
      { clientId = "",
        clientSecret = "",
        scopes = Just []
      }

instance FromJSON EnvConfigAuthParams

type EnvConfig = KeyMap EnvConfigAuthParams

-- * type class for defining a IDP

type family IDPUserInfo a

type family IDPName a

data IDP a = IDP
  { idpName :: IDPName a,
    oauth2Config :: OAuth2,
    oauth2Scopes :: [TL.Text],
    oauth2AuthorizeParams :: [(BS.ByteString, BS.ByteString)],
    oauth2UserInfoUri :: URI,
    oauth2FetchAccessToken :: Manager -> OAuth2 -> ExchangeToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token,
    oauth2RefreshAccessToken :: Manager -> OAuth2 -> RefreshToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token,
    oauth2FetchUserInfo :: FromJSON (IDPUserInfo a) => IDP a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO (IDPUserInfo a),
    convertUserInfoToLoginUser :: IDPUserInfo a -> LoginUser
  }

instance Default (IDP a) where
  def =
    IDP
      { idpName = undefined,
        oauth2Config = def,
        oauth2Scopes = [],
        oauth2AuthorizeParams = [],
        oauth2UserInfoUri = [uri|https://example.com|],
        oauth2FetchAccessToken = fetchAccessToken,
        oauth2RefreshAccessToken = refreshAccessToken,
        oauth2FetchUserInfo = fetchUserInfoViaGet,
        convertUserInfoToLoginUser = const (LoginUser "")
      }

instance (Show (IDPName a)) => Eq (IDP a) where
  x == y = getIdpName x == getIdpName y

class IsIDP a where
  idpLabel :: a -> IDPLabel
  authUri :: a -> TL.Text
  tokenReq :: a -> Manager -> ExchangeToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token
  tokenRefreshReq :: a -> Manager -> RefreshToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token
  userReq :: a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO LoginUser

instance (Show (IDPName a), FromJSON (IDPUserInfo a)) => IsIDP (IDP a) where
  idpLabel = getIdpName
  authUri = createAuthorizeUri
  tokenReq IDP {..} mgr = oauth2FetchAccessToken mgr oauth2Config
  tokenRefreshReq IDP {..} mgr = oauth2RefreshAccessToken mgr oauth2Config
  userReq idp@IDP {..} mgr at = do
    resp <- oauth2FetchUserInfo idp mgr at
    return $ convertUserInfoToLoginUser resp

getIdpName :: Show (IDPName a) => IDP a -> TL.Text
getIdpName = TL.pack . show . idpName

fetchUserInfoViaGet :: FromJSON (IDPUserInfo a) => IDP a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO (IDPUserInfo a)
fetchUserInfoViaGet i2 mgr at = authGetJSONWithAuthMethod AuthInRequestHeader mgr at (oauth2UserInfoUri i2)

fetchUserInfoViaPost :: FromJSON (IDPUserInfo a) => IDP a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO (IDPUserInfo a)
fetchUserInfoViaPost i2 mgr at = authPostJSONWithAuthMethod AuthInRequestHeader mgr at (oauth2UserInfoUri i2) []

createAuthorizeUri :: (Show (IDPName a)) => IDP a -> TL.Text
createAuthorizeUri idp@IDP {..} =
  createCodeUri (defaultAuthorizeParam idp ++ oauth2AuthorizeParams) oauth2Config
  where
    createCodeUri allParams key =
      TL.fromStrict $
        T.decodeUtf8 $
          serializeURIRef' $
              authorizationUrlWithParams allParams key

defaultAuthorizeParam :: (Show (IDPName a)) => IDP a -> [(BS.ByteString, BS.ByteString)]
defaultAuthorizeParam IDP {..} =
  [ ("state", tlToBS $ TL.pack $ show idpName <> ".test-state-123"),
    ("scope", tlToBS $ TL.intercalate " " oauth2Scopes)
  ]

defaultOAuth2RedirectUri :: URI
defaultOAuth2RedirectUri = [uri|http://localhost:9988/oauth2/callback|]

-- Heterogenous collections
-- https://wiki.haskell.org/Heterogenous_collections
--
data IDPApp = forall a. IsIDP a => IDPApp a

newtype LoginUser = LoginUser
  { loginUserName :: TL.Text
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
