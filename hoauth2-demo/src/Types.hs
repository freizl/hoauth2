{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Control.Concurrent.MVar
import Data.Aeson
import Data.Default
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Env
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Text.Mustache ((~>))
import Text.Mustache qualified as M
import User

-------------------------------------------------------------------------------
--                                  DemoIdp                                  --
-------------------------------------------------------------------------------

data DemoIdp
  = forall i.
    ( HasDemoLoginUser i
    , FromJSON (IdpUser i)
    ) =>
    DemoIdp (Idp i)

-------------------------------------------------------------------------------
--                                   AppEnv                                  --
-------------------------------------------------------------------------------
type TenantBasedIdps = (Idp Auth0, Idp Okta)

data AppEnv = AppEnv
  { oauthAppSettings :: OAuthAppSettings
  , oidcIdps :: TenantBasedIdps
  , findIdpByName :: IdpName -> DemoIdp
  , sessionStore :: AuthorizationGrantUserStore
  }

-------------------------------------------------------------------------------
--                                  Session                                  --
-------------------------------------------------------------------------------
newtype AuthorizationGrantUserStore = AuthorizationGrantUserStore (MVar (Map.Map IdpName IdpAuthorizationCodeAppSessionData))

data IdpAuthorizationCodeAppSessionData = IdpAuthorizationCodeAppSessionData
  { idpName :: IdpName
  , loginUser :: Maybe DemoLoginUser
  , oauth2Token :: Maybe OAuth2Token
  , authorizePkceCodeVerifier :: Maybe CodeVerifier
  , authorizeAbsUri :: TL.Text
  }

instance Default IdpAuthorizationCodeAppSessionData where
  def =
    IdpAuthorizationCodeAppSessionData
      { idpName = Okta
      , loginUser = Nothing
      , oauth2Token = Nothing
      , authorizePkceCodeVerifier = Nothing
      , authorizeAbsUri = ""
      }

instance M.ToMustache IdpAuthorizationCodeAppSessionData where
  toMustache (IdpAuthorizationCodeAppSessionData {..}) = do
    let hasDeviceGrant = idpName `elem` [Okta, GitHub, Auth0, AzureAD, Google]
        hasClientCredentialsGrant = idpName `elem` [Okta, Auth0]
        hasPasswordGrant = idpName `elem` [Okta, Auth0]
    M.object
      [ "isLogin" ~> isJust loginUser
      , "user" ~> loginUser
      , "idpName" ~> idpName
      , "hasDeviceGrant" ~> hasDeviceGrant
      , "hasClientCredentialsGrant" ~> hasClientCredentialsGrant
      , "hasPasswordGrant" ~> hasPasswordGrant
      ]

-------------------------------------------------------------------------------
--                             IdpName extension                             --
-------------------------------------------------------------------------------

allIdpNames :: [IdpName]
allIdpNames = [minBound .. maxBound]

deriving instance Read IdpName

fromText :: Text -> IdpName
fromText = read . TL.unpack

toText :: IdpName -> Text
toText = TL.pack . show

instance M.ToMustache IdpName where
  toMustache idpName = M.toMustache (show idpName)
