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

data IDP = IDP
  { idpName :: TL.Text,
    oauth2Config :: OAuth2,
    oauth2Scopes :: [TL.Text],
    oauth2UserInfoUri :: URI
  }

type family IDPUserInfo a

data IDP2 a = IDP2
  { idpName2 :: TL.Text,
    oauth2Config2 :: OAuth2,
    oauth2Scopes2 :: [TL.Text],
    oauth2UserInfoUri2 :: URI,
    oauth2FetchAccessToken :: Manager -> OAuth2 -> ExchangeToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token,
    oauth2RefreshAccessToken :: Manager -> OAuth2 -> RefreshToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token,
    oauth2FetchUserInfo :: forall b. FromJSON b => IDP2 a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO b,
    convertUserInfoToLoginUser :: IDPUserInfo a -> LoginUser
  }

instance Default (IDP2 a) where
  def =
    IDP2
      { idpName2 = "",
        oauth2Config2 = def,
        oauth2Scopes2 = [],
        oauth2UserInfoUri2 = [uri|https://example.com|],
        oauth2FetchAccessToken = fetchAccessToken,
        oauth2RefreshAccessToken = refreshAccessToken,
        oauth2FetchUserInfo = fetchUserInfoViaGet,
        convertUserInfoToLoginUser = const (LoginUser "")
      }

fetchUserInfoViaGet :: FromJSON b => IDP2 a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO b
fetchUserInfoViaGet i2 mgr at = authGetJSON mgr at (oauth2UserInfoUri2 i2)

fetchUserInfoViaPost :: FromJSON b => IDP2 a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO b
fetchUserInfoViaPost i2 mgr at = authPostJSON mgr at (oauth2UserInfoUri2 i2) []

test :: (FromJSON (IDPUserInfo a)) => IDP2 a -> ExceptT BSL.ByteString IO LoginUser
test idp2@IDP2{..} = do
  resp <- oauth2FetchUserInfo idp2 undefined undefined
  return (convertUserInfoToLoginUser resp)

instance Eq IDP where
  x == y = idpName x == idpName y

instance HasLabel IDP where
  idpLabel = idpName

instance HasAuthUri IDP where
  authUri = createAuthorizeUri

instance HasTokenReq IDP where
  tokenReq IDP {..} mgr = fetchAccessToken mgr oauth2Config

instance HasTokenRefreshReq IDP where
  tokenRefreshReq IDP {..} mgr = refreshAccessToken mgr oauth2Config

createAuthorizeUri :: IDP -> TL.Text
createAuthorizeUri idp@IDP {..} = createCodeUri oauth2Config (defaultAuthorizeParam idp)
  where
    createCodeUri key params =
      TL.fromStrict $
        T.decodeUtf8 $
          serializeURIRef' $
            appendQueryParams params $
              authorizationUrl key

defaultAuthorizeParam :: IDP -> [(BS.ByteString, BS.ByteString)]
defaultAuthorizeParam IDP {..} =
  [ ("state", tlToBS $ idpName <> ".test-state-123"),
    ("scope", tlToBS $ TL.intercalate " " oauth2Scopes)
  ]

defaultOAuth2RedirectUri :: Maybe URI
defaultOAuth2RedirectUri = Just [uri|http://localhost:9988/oauth2/callback|]

class HasLabel a where
  idpLabel :: a -> IDPLabel

class HasAuthUri a where
  authUri :: a -> TL.Text

class HasAuthorizeExtraParam a where
  authorizeParam :: a -> [(BS.ByteString, BS.ByteString)]
  authorizeParam _ = []

class HasTokenReq a where
  tokenReq :: a -> Manager -> ExchangeToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token

class HasTokenRefreshReq a where
  tokenRefreshReq :: a -> Manager -> RefreshToken -> ExceptT (OAuth2Error TR.Errors) IO OAuth2Token

-- | TODO: associates userInfo uri and toLoginUser method
-- so that can have default implementation for userReq
class HasUserReq a where
  userReq :: a -> Manager -> AccessToken -> ExceptT BSL.ByteString IO LoginUser

-- data IDPApp = IDPApp IDP

-- Heterogenous collections
-- https://wiki.haskell.org/Heterogenous_collections
--
data IDPApp
  = forall a.
    ( HasTokenRefreshReq a,
      HasTokenReq a,
      HasUserReq a,
      HasAuthUri a,
      HasLabel a
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
