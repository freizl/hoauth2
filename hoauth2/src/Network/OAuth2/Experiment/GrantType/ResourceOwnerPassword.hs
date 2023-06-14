{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.GrantType.ResourceOwnerPassword where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..), OAuth2 (..))
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Types

data Application = Application
  { ropClientId :: ClientId
  , ropClientSecret :: ClientSecret
  , ropName :: Text
  , ropScope :: Set Scope
  , ropUserName :: Username
  , ropPassword :: Password
  , ropTokenRequestExtraParams :: Map Text Text
  }

instance HasOAuth2Key Application where
  mkOAuth2Key :: Application -> OAuth2
  mkOAuth2Key Application {..} = toOAuth2Key ropClientId ropClientSecret

instance HasTokenRequestClientAuthenticationMethod Application where
  getClientAuthenticationMethod :: Application -> ClientAuthenticationMethod
  getClientAuthenticationMethod _ = ClientSecretBasic

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.3.2
instance HasTokenRequest Application where
  type ExchangeTokenInfo Application = ()

  data TokenRequest Application = PasswordTokenRequest
    { trScope :: Set Scope
    , trUsername :: Username
    , trPassword :: Password
    , trGrantType :: GrantTypeValue
    , trExtraParams :: Map Text Text
    }
  mkTokenRequestParam :: Application -> () -> TokenRequest Application
  mkTokenRequestParam Application {..} _ =
    PasswordTokenRequest
      { trUsername = ropUserName
      , trPassword = ropPassword
      , trGrantType = GTPassword
      , trScope = ropScope
      , trExtraParams = ropTokenRequestExtraParams
      }

instance ToQueryParam (TokenRequest Application) where
  toQueryParam :: TokenRequest Application -> Map Text Text
  toQueryParam PasswordTokenRequest {..} =
    Map.unions
      [ toQueryParam trGrantType
      , toQueryParam trScope
      , toQueryParam trUsername
      , toQueryParam trPassword
      , trExtraParams
      ]

instance HasUserInfoRequest Application

instance HasRefreshTokenRequest Application where
  data RefreshTokenRequest Application = ResourceOwnerPasswordRefreshTokenRequest
    { rrRefreshToken :: OAuth2.RefreshToken
    , rrGrantType :: GrantTypeValue
    , rrScope :: Set Scope
    }

  mkRefreshTokenRequestParam :: Application -> OAuth2.RefreshToken -> RefreshTokenRequest Application
  mkRefreshTokenRequestParam Application {..} rt =
    ResourceOwnerPasswordRefreshTokenRequest
      { rrScope = ropScope
      , rrGrantType = GTRefreshToken
      , rrRefreshToken = rt
      }

instance ToQueryParam (RefreshTokenRequest Application) where
  toQueryParam :: RefreshTokenRequest Application -> Map Text Text
  toQueryParam ResourceOwnerPasswordRefreshTokenRequest {..} =
    Map.unions
      [ toQueryParam rrGrantType
      , toQueryParam rrScope
      , toQueryParam rrRefreshToken
      ]
