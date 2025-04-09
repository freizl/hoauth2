{-# LANGUAGE FlexibleInstances #-}

module Network.OAuth2.Experiment.Grants.AuthorizationCode where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth2 (ClientAuthenticationMethod (..), ExchangeToken (..))
import Network.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Flows.AuthorizationRequest
import Network.OAuth2.Experiment.Flows.RefreshTokenRequest
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Utils
import URI.ByteString hiding (UserInfo)

-- | An Application that supports "Authorization code" flow
--
-- https://www.rfc-editor.org/rfc/rfc6749#section-4.1
data AuthorizationCodeApplication = AuthorizationCodeApplication
  { acName :: Text
  , acClientId :: ClientId
  , acClientSecret :: ClientSecret
  , acScope :: Set Scope
  , acRedirectUri :: URI
  , acAuthorizeState :: AuthorizeState
  , acAuthorizeRequestExtraParams :: Map Text Text
  , acClientAuthenticationMethod :: ClientAuthenticationMethod
  }

instance HasClientAuthenticationMethod AuthorizationCodeApplication where
  getClientAuthenticationMethod :: AuthorizationCodeApplication -> ClientAuthenticationMethod
  getClientAuthenticationMethod AuthorizationCodeApplication {..} = acClientAuthenticationMethod
  addClientAuthToHeader AuthorizationCodeApplication {..} = addSecretToHeader acClientId acClientSecret

mkAuthorizationRequestParam :: AuthorizationCodeApplication -> AuthorizationRequestParam
mkAuthorizationRequestParam AuthorizationCodeApplication {..} =
  AuthorizationRequestParam
    { arScope = acScope
    , arState = acAuthorizeState
    , arClientId = acClientId
    , arRedirectUri = Just (RedirectUri acRedirectUri)
    , arResponseType = Code
    , arExtraParams = acAuthorizeRequestExtraParams
    }

mkPkceAuthorizeRequestParam :: MonadIO m => AuthorizationCodeApplication -> m (AuthorizationRequestParam, CodeVerifier)
mkPkceAuthorizeRequestParam app = do
  PkceRequestParam {..} <- mkPkceParam
  let authReqParam = mkAuthorizationRequestParam app
      combinatedExtraParams =
        Map.unions
          [ arExtraParams authReqParam
          , toQueryParam codeChallenge
          , toQueryParam codeChallengeMethod
          ]
  pure (authReqParam {arExtraParams = combinatedExtraParams}, codeVerifier)

-- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.3
instance HasTokenRequest AuthorizationCodeApplication where
  type ExchangeTokenInfo AuthorizationCodeApplication = ExchangeToken
  data TokenRequest AuthorizationCodeApplication = AuthorizationCodeTokenRequest
    { trCode :: ExchangeToken
    , trGrantType :: GrantTypeValue
    , trRedirectUri :: RedirectUri
    , trClientId :: ClientId
    , trClientSecret :: ClientSecret
    , trClientAuthenticationMethod :: ClientAuthenticationMethod
    }

  mkTokenRequestParam :: AuthorizationCodeApplication -> ExchangeToken -> TokenRequest AuthorizationCodeApplication
  mkTokenRequestParam AuthorizationCodeApplication {..} authCode =
    AuthorizationCodeTokenRequest
      { trCode = authCode
      , trGrantType = GTAuthorizationCode
      , trRedirectUri = RedirectUri acRedirectUri
      , trClientId = acClientId
      , trClientSecret = acClientSecret
      , trClientAuthenticationMethod = acClientAuthenticationMethod
      }

instance ToQueryParam (TokenRequest AuthorizationCodeApplication) where
  toQueryParam :: TokenRequest AuthorizationCodeApplication -> Map Text Text
  toQueryParam AuthorizationCodeTokenRequest {..} =
    let extraBodyBasedOnClientAuthMethod =
          case trClientAuthenticationMethod of
            ClientAssertionJwt ->
              [ Map.fromList
                  [ ("client_assertion_type", "urn:ietf:params:oauth:client-assertion-type:jwt-bearer")
                  , ("client_assertion", bs8ToLazyText $ tlToBS $ unClientSecret trClientSecret)
                  ]
              ]
            ClientSecretPost -> [toQueryParam trClientId, toQueryParam trClientSecret]
            ClientSecretBasic -> []
     in Map.unions $
          [ toQueryParam trCode
          , toQueryParam trGrantType
          , toQueryParam trRedirectUri
          ]
            ++ extraBodyBasedOnClientAuthMethod

instance HasUserInfoRequest AuthorizationCodeApplication

instance HasRefreshTokenRequest AuthorizationCodeApplication where
  mkRefreshTokenRequestParam :: AuthorizationCodeApplication -> OAuth2.RefreshToken -> RefreshTokenRequest
  mkRefreshTokenRequestParam AuthorizationCodeApplication {..} rt =
    RefreshTokenRequest
      { rrScope = acScope
      , rrGrantType = GTRefreshToken
      , rrRefreshToken = rt
      , rrClientId = if acClientAuthenticationMethod == ClientSecretPost then Just acClientId else Nothing
      , rrClientSecret = if acClientAuthenticationMethod == ClientSecretPost then Just acClientSecret else Nothing
      }
