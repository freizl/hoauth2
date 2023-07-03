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

-- | An Application that supports "Resource Owner Password" flow
--
-- https://www.rfc-editor.org/rfc/rfc6749#section-4.3
data ResourceOwnerPasswordApplication = ResourceOwnerPasswordApplication
  { ropClientId :: ClientId
  , ropClientSecret :: ClientSecret
  , ropName :: Text
  , ropScope :: Set Scope
  , ropUserName :: Username
  , ropPassword :: Password
  , ropTokenRequestExtraParams :: Map Text Text
  }

instance HasOAuth2Key ResourceOwnerPasswordApplication where
  mkOAuth2Key :: ResourceOwnerPasswordApplication -> OAuth2
  mkOAuth2Key ResourceOwnerPasswordApplication {..} = toOAuth2Key ropClientId ropClientSecret

instance HasTokenRequestClientAuthenticationMethod ResourceOwnerPasswordApplication where
  getClientAuthenticationMethod :: ResourceOwnerPasswordApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod _ = ClientSecretBasic

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.3.2
instance HasTokenRequest ResourceOwnerPasswordApplication where
  type ExchangeTokenInfo ResourceOwnerPasswordApplication = NoNeedExchangeToken

  data TokenRequest ResourceOwnerPasswordApplication = PasswordTokenRequest
    { trScope :: Set Scope
    , trUsername :: Username
    , trPassword :: Password
    , trGrantType :: GrantTypeValue
    , trExtraParams :: Map Text Text
    }
  mkTokenRequestParam :: ResourceOwnerPasswordApplication -> NoNeedExchangeToken -> TokenRequest ResourceOwnerPasswordApplication
  mkTokenRequestParam ResourceOwnerPasswordApplication {..} _ =
    PasswordTokenRequest
      { trUsername = ropUserName
      , trPassword = ropPassword
      , trGrantType = GTPassword
      , trScope = ropScope
      , trExtraParams = ropTokenRequestExtraParams
      }

instance ToQueryParam (TokenRequest ResourceOwnerPasswordApplication) where
  toQueryParam :: TokenRequest ResourceOwnerPasswordApplication -> Map Text Text
  toQueryParam PasswordTokenRequest {..} =
    Map.unions
      [ toQueryParam trGrantType
      , toQueryParam trScope
      , toQueryParam trUsername
      , toQueryParam trPassword
      , trExtraParams
      ]

instance HasUserInfoRequest ResourceOwnerPasswordApplication

instance HasRefreshTokenRequest ResourceOwnerPasswordApplication where
  data RefreshTokenRequest ResourceOwnerPasswordApplication = ResourceOwnerPasswordRefreshTokenRequest
    { rrRefreshToken :: OAuth2.RefreshToken
    , rrGrantType :: GrantTypeValue
    , rrScope :: Set Scope
    }

  mkRefreshTokenRequestParam :: ResourceOwnerPasswordApplication -> OAuth2.RefreshToken -> RefreshTokenRequest ResourceOwnerPasswordApplication
  mkRefreshTokenRequestParam ResourceOwnerPasswordApplication {..} rt =
    ResourceOwnerPasswordRefreshTokenRequest
      { rrScope = ropScope
      , rrGrantType = GTRefreshToken
      , rrRefreshToken = rt
      }

instance ToQueryParam (RefreshTokenRequest ResourceOwnerPasswordApplication) where
  toQueryParam :: RefreshTokenRequest ResourceOwnerPasswordApplication -> Map Text Text
  toQueryParam ResourceOwnerPasswordRefreshTokenRequest {..} =
    Map.unions
      [ toQueryParam rrGrantType
      , toQueryParam rrScope
      , toQueryParam rrRefreshToken
      ]
