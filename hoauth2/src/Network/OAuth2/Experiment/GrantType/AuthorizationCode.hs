{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.GrantType.AuthorizationCode where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth.OAuth2 (ClientAuthenticationMethod (..), ExchangeToken (..), OAuth2)
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.AuthorizationRequest
import Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
import URI.ByteString hiding (UserInfo)

data Application = Application
  { acName :: Text
  , acClientId :: ClientId
  , acClientSecret :: ClientSecret
  , acScope :: Set Scope
  , acRedirectUri :: URI
  , acAuthorizeState :: AuthorizeState
  , acAuthorizeRequestExtraParams :: Map Text Text
  , acTokenRequestAuthenticationMethod :: ClientAuthenticationMethod
  }

instance HasOAuth2Key Application where
  mkOAuth2Key :: Application -> OAuth2
  mkOAuth2Key Application {..} = toOAuth2Key acClientId acClientSecret

instance HasTokenRequestClientAuthenticationMethod Application where
  getClientAuthenticationMethod :: Application -> ClientAuthenticationMethod
  getClientAuthenticationMethod Application {..} = acTokenRequestAuthenticationMethod

instance HasAuthorizeRequest Application where
  mkAuthorizeRequestParam :: Application -> AuthorizationRequestParam
  mkAuthorizeRequestParam Application {..} =
    AuthorizationRequestParam
      { arScope = acScope
      , arState = acAuthorizeState
      , arClientId = acClientId
      , arRedirectUri = Just (RedirectUri acRedirectUri)
      , arResponseType = Code
      , arExtraParams = acAuthorizeRequestExtraParams
      }

instance HasPkceAuthorizeRequest Application where
  mkPkceAuthorizeRequestParam :: MonadIO m => Application -> m (AuthorizationRequestParam, CodeVerifier)
  mkPkceAuthorizeRequestParam app = do
    PkceRequestParam {..} <- mkPkceParam
    let authReqParam = mkAuthorizeRequestParam app
        combinatedExtraParams =
          Map.unions
            [ arExtraParams authReqParam
            , toQueryParam codeChallenge
            , toQueryParam codeChallengeMethod
            ]
    pure (authReqParam {arExtraParams = combinatedExtraParams}, codeVerifier)

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.3
instance HasTokenRequest Application where
  type ExchangeTokenInfo Application = ExchangeToken
  data TokenRequest Application = AuthorizationCodeTokenRequest
    { trCode :: ExchangeToken
    , trGrantType :: GrantTypeValue
    , trRedirectUri :: RedirectUri
    }

  mkTokenRequestParam :: Application -> ExchangeToken -> TokenRequest Application
  mkTokenRequestParam Application {..} authCode =
    AuthorizationCodeTokenRequest
      { trCode = authCode
      , trGrantType = GTAuthorizationCode
      , trRedirectUri = RedirectUri acRedirectUri
      }

instance ToQueryParam (TokenRequest Application) where
  toQueryParam :: TokenRequest Application -> Map Text Text
  toQueryParam AuthorizationCodeTokenRequest {..} =
    Map.unions
      [ toQueryParam trCode
      , toQueryParam trGrantType
      , toQueryParam trRedirectUri
      ]

instance HasUserInfoRequest Application

instance HasRefreshTokenRequest Application where
  data RefreshTokenRequest Application = AuthorizationCodeTokenRefreshRequest
    { rrRefreshToken :: OAuth2.RefreshToken
    , rrGrantType :: GrantTypeValue
    , rrScope :: Set Scope
    }

  mkRefreshTokenRequestParam :: Application -> OAuth2.RefreshToken -> RefreshTokenRequest Application
  mkRefreshTokenRequestParam Application {..} rt =
    AuthorizationCodeTokenRefreshRequest
      { rrScope = acScope
      , rrGrantType = GTRefreshToken
      , rrRefreshToken = rt
      }

instance ToQueryParam (RefreshTokenRequest Application) where
  toQueryParam :: RefreshTokenRequest Application -> Map Text Text
  toQueryParam AuthorizationCodeTokenRefreshRequest {..} =
    Map.unions
      [ toQueryParam rrGrantType
      , toQueryParam rrScope
      , toQueryParam rrRefreshToken
      ]
