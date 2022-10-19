{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.OAuth2.Experiment.Types where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson (FromJSON)
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Default (Default (def))
import Data.Kind
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text.Encoding qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth.OAuth2.TokenRequest qualified as TR
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Utils
import URI.ByteString hiding (UserInfo)

{- NOTE
  1. shall I lift the constrain of all 'a :: GrantTypeFlow' so that user has max customization/flexibility?
-}

-------------------------------------------------------------------------------

-- * Grant Type

-------------------------------------------------------------------------------

data GrantTypeFlow = AuthorizationCode | ResourceOwnerPassword | ClientCredentials | JwtBearer

-------------------------------------------------------------------------------

-- * Response Type value

-------------------------------------------------------------------------------

class ToResponseTypeValue (a :: GrantTypeFlow) where
  toResponseTypeValue :: IsString b => b

instance ToResponseTypeValue 'AuthorizationCode where
  -- https://www.rfc-editor.org/rfc/rfc6749#section-3.1.1
  -- Only support "authorization code" flow
  toResponseTypeValue :: IsString b => b
  toResponseTypeValue = "code"

toResponseTypeParam :: forall a b req. (ToResponseTypeValue a, IsString b) => req a -> Map b b
toResponseTypeParam _ = Map.singleton "response_type" (toResponseTypeValue @a)

-------------------------------------------------------------------------------

-- * Grant Type value

-------------------------------------------------------------------------------

newtype UrnOAuthParam a = UrnOAuthParam a

-- | Grant type query parameter has association with 'GrantTypeFlow' but not completely strict.
--
-- e.g. Both 'AuthorizationCode' and 'ResourceOwnerPassword' flow could support refresh token flow.
data GrantTypeValue
  = GTAuthorizationCode
  | GTPassword
  | GTClientCredentials
  | GTRefreshToken
  | GTJwtBearer
  deriving (Eq, Show)

-------------------------------------------------------------------------------

-- * Scope

-------------------------------------------------------------------------------

-- TODO: following data type is not ideal as Idp would have lots of 'Custom Text'
--
-- @
-- data Scope = OPENID | PROFILE | EMAIL | OFFLINE_ACCESS | Custom Text
-- @
--
-- Would be nice to define Enum for standard Scope, plus allow user to define their own define (per Idp) and plugin somehow.
newtype Scope = Scope {unScope :: Text}
  deriving (Show, Eq, Ord)

instance IsString Scope where
  fromString :: String -> Scope
  fromString = Scope . TL.pack

-------------------------------------------------------------------------------

-- * Credentials

-------------------------------------------------------------------------------
newtype ClientId = ClientId {unClientId :: Text}
  deriving (Show, Eq, IsString)

newtype ClientSecret = ClientSecret {unClientSecret :: Text}
  deriving (Eq, IsString)

-- | In order to reuse some methods from legacy "Network.OAuth.OAuth2".
-- Will be removed when Experiment module becomes default.
toOAuth2Key :: ClientId -> ClientSecret -> OAuth2
toOAuth2Key cid csecret =
  def
    { oauth2ClientId = TL.toStrict $ unClientId cid
    , oauth2ClientSecret = TL.toStrict $ unClientSecret csecret
    }

newtype RedirectUri = RedirectUri {unRedirectUri :: URI}
  deriving (Eq)

newtype AuthorizeState = AuthorizeState {unAuthorizeState :: Text}
  deriving (Eq)

instance IsString AuthorizeState where
  fromString :: String -> AuthorizeState
  fromString = AuthorizeState . TL.pack

newtype Username = Username {unUsername :: Text}
  deriving (Eq)

instance IsString Username where
  fromString :: String -> Username
  fromString = Username . TL.pack

newtype Password = Password {unPassword :: Text}
  deriving (Eq)

instance IsString Password where
  fromString :: String -> Password
  fromString = Password . TL.pack

-------------------------------------------------------------------------------

-- * Query parameters

-------------------------------------------------------------------------------
class ToQueryParam a where
  toQueryParam :: a -> Map Text Text

instance ToQueryParam a => ToQueryParam (Maybe a) where
  toQueryParam :: ToQueryParam a => Maybe a -> Map Text Text
  toQueryParam Nothing = Map.empty
  toQueryParam (Just a) = toQueryParam a

instance ToQueryParam GrantTypeValue where
  toQueryParam :: GrantTypeValue -> Map Text Text
  toQueryParam x = Map.singleton "grant_type" (val x)
    where
      val :: GrantTypeValue -> Text
      val GTAuthorizationCode = "authorization_code"
      val GTPassword = "password"
      val GTClientCredentials = "client_credentials"
      val GTRefreshToken = "refresh_token"
      val GTJwtBearer = "urn:ietf:params:oauth:grant-type:jwt-bearer"

instance ToQueryParam ClientId where
  toQueryParam :: ClientId -> Map Text Text
  toQueryParam (ClientId i) = Map.singleton "client_id" i

instance ToQueryParam ClientSecret where
  toQueryParam :: ClientSecret -> Map Text Text
  toQueryParam (ClientSecret x) = Map.singleton "client_secret" x

instance ToQueryParam Username where
  toQueryParam :: Username -> Map Text Text
  toQueryParam (Username x) = Map.singleton "username" x

instance ToQueryParam Password where
  toQueryParam :: Password -> Map Text Text
  toQueryParam (Password x) = Map.singleton "password" x

instance ToQueryParam AuthorizeState where
  toQueryParam :: AuthorizeState -> Map Text Text
  toQueryParam (AuthorizeState x) = Map.singleton "state" x

instance ToQueryParam RedirectUri where
  toQueryParam (RedirectUri uri) = Map.singleton "redirect_uri" (bs8ToLazyText $ serializeURIRef' uri)

instance ToQueryParam (Set Scope) where
  toQueryParam :: Set Scope -> Map Text Text
  toQueryParam = toScopeParam . Set.map unScope
    where
      toScopeParam :: (IsString a) => Set Text -> Map a Text
      toScopeParam scope = Map.singleton "scope" (TL.intercalate " " $ Set.toList scope)

instance ToQueryParam CodeVerifier where
  toQueryParam :: CodeVerifier -> Map Text Text
  toQueryParam (CodeVerifier x) = Map.singleton "code_verifier" (TL.fromStrict x)

instance ToQueryParam CodeChallenge where
  toQueryParam :: CodeChallenge -> Map Text Text
  toQueryParam (CodeChallenge x) = Map.singleton "code_challenge" (TL.fromStrict x)

instance ToQueryParam CodeChallengeMethod where
  toQueryParam :: CodeChallengeMethod -> Map Text Text
  toQueryParam x = Map.singleton "code_challenge_method" (TL.pack $ show x)

instance ToQueryParam ExchangeToken where
  toQueryParam :: ExchangeToken -> Map Text Text
  toQueryParam (ExchangeToken x) = Map.singleton "code" (TL.fromStrict x)

instance ToQueryParam OAuth2.RefreshToken where
  toQueryParam :: OAuth2.RefreshToken -> Map Text Text
  toQueryParam (OAuth2.RefreshToken x) = Map.singleton "refresh_token" (TL.fromStrict x)

-------------------------------------------------------------------------------

-- * Authorization and Token Requests types

-------------------------------------------------------------------------------

class HasIdpAppName (a :: GrantTypeFlow) where
  getIdpAppName :: IdpApplication a i -> Text

class HasAuthorizeRequest (a :: GrantTypeFlow) where
  data AuthorizationRequest a
  type MkAuthorizationRequestResponse a
  mkAuthorizeRequestParameter :: IdpApplication a i -> AuthorizationRequest a
  mkAuthorizeRequest :: IdpApplication a i -> MkAuthorizationRequestResponse a

class HasTokenRequest (a :: GrantTypeFlow) where
  -- | Each GrantTypeFlow has slightly different request parameter to /token endpoint.
  data TokenRequest a

  -- | Only 'AuthorizationCode flow (but not resource owner password nor client credentials) will use 'ExchangeToken' in the token request
  -- create type family to be explicit on it.
  -- with 'type instance WithExchangeToken a b = b' implies no exchange token
  -- v.s. 'type instance WithExchangeToken a b = ExchangeToken -> b' implies needing an exchange token
  type WithExchangeToken a b

  mkTokenRequest ::
    IdpApplication a i ->
    WithExchangeToken a (TokenRequest a)

  conduitTokenRequest ::
    (MonadIO m) =>
    IdpApplication a i ->
    Manager ->
    WithExchangeToken a (ExceptT (OAuth2Error TR.Errors) m OAuth2Token)

class HasPkceAuthorizeRequest (a :: GrantTypeFlow) where
  mkPkceAuthorizeRequest :: MonadIO m => IdpApplication a i -> m (TL.Text, CodeVerifier)

class HasPkceTokenRequest (b :: GrantTypeFlow) where
  conduitPkceTokenRequest ::
    (MonadIO m) =>
    IdpApplication b i ->
    Manager ->
    (ExchangeToken, CodeVerifier) ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token

class HasRefreshTokenRequest (a :: GrantTypeFlow) where
  -- | https://www.rfc-editor.org/rfc/rfc6749#page-47
  data RefreshTokenRequest a

  mkRefreshTokenRequest :: IdpApplication a i -> OAuth2.RefreshToken -> RefreshTokenRequest a
  conduitRefreshTokenRequest ::
    (MonadIO m) =>
    IdpApplication a i ->
    Manager ->
    OAuth2.RefreshToken ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token

-------------------------------------------------------------------------------

-- * User Info types

-------------------------------------------------------------------------------

type family IdpUserInfo a

class HasUserInfoRequest (a :: GrantTypeFlow) where
  conduitUserInfoRequest ::
    FromJSON (IdpUserInfo i) =>
    IdpApplication a i ->
    Manager ->
    AccessToken ->
    ExceptT BSL.ByteString IO (IdpUserInfo i)

-------------------------------------------------------------------------------

-- * Idp App

-------------------------------------------------------------------------------

-- | Shall IdpApplication has a field of 'Idp a'??
data Idp a = Idp
  { idpUserInfoEndpoint :: URI
  , -- NOTE: maybe worth data type to distinguish authorize and token endpoint
    -- as I made mistake at passing to Authorize and Token Request
    idpAuthorizeEndpoint :: URI
  , idpTokenEndpoint :: URI
  , idpFetchUserInfo ::
      forall m.
      (FromJSON (IdpUserInfo a), MonadIO m) =>
      Manager ->
      AccessToken ->
      URI ->
      ExceptT BSL.ByteString m (IdpUserInfo a)
  }

-------------------------------------------------------------------------------

-- * Idp App Config

-------------------------------------------------------------------------------

data family IdpApplication (a :: GrantTypeFlow) (i :: Type)

-------------------------------------------------------------------------------

-- * Authorization Code flow

-------------------------------------------------------------------------------

-- | An Application that supports "Authorization code" flow
data instance IdpApplication 'AuthorizationCode i = AuthorizationCodeIdpApplication
  { idpAppName :: Text
  , idpAppClientId :: ClientId
  , idpAppClientSecret :: ClientSecret
  , idpAppScope :: Set Scope
  , idpAppRedirectUri :: URI
  , idpAppAuthorizeState :: AuthorizeState
  , idpAppAuthorizeExtraParams :: Map Text Text
  -- ^ Though technically one key can have multiple value in query, but who actually does it?!
  , idpAppTokenRequestAuthenticationMethod :: ClientAuthenticationMethod
  , idp :: Idp i
  }

-- NOTE: maybe add function for parase authorization response
-- though seems overkill. https://github.com/freizl/hoauth2/issues/149
-- parseAuthorizationResponse :: String -> AuthorizationResponse
-- parseAuthorizationResponse :: ( String, String ) -> AuthorizationResponse

instance HasIdpAppName 'AuthorizationCode where
  getIdpAppName :: IdpApplication 'AuthorizationCode i -> Text
  getIdpAppName AuthorizationCodeIdpApplication {..} = idpAppName

instance HasAuthorizeRequest 'AuthorizationCode where
  -- \| https://www.rfc-editor.org/rfc/rfc6749#section-4.1.1
  data AuthorizationRequest 'AuthorizationCode = AuthorizationCodeAuthorizationRequest
    { scope :: Set Scope
    , state :: AuthorizeState
    , clientId :: ClientId
    , redirectUri :: Maybe RedirectUri
    }
  type MkAuthorizationRequestResponse 'AuthorizationCode = Text

  mkAuthorizeRequestParameter :: IdpApplication 'AuthorizationCode i -> AuthorizationRequest 'AuthorizationCode
  mkAuthorizeRequestParameter AuthorizationCodeIdpApplication {..} =
    AuthorizationCodeAuthorizationRequest
      { scope = if null idpAppScope then Set.empty else idpAppScope
      , state = idpAppAuthorizeState
      , clientId = idpAppClientId
      , redirectUri = Just (RedirectUri idpAppRedirectUri)
      }

  mkAuthorizeRequest :: IdpApplication 'AuthorizationCode i -> Text
  mkAuthorizeRequest idpAppConfig@AuthorizationCodeIdpApplication {..} =
    let req = mkAuthorizeRequestParameter idpAppConfig
        allParams =
          map (bimap tlToBS tlToBS) $
            Map.toList $
              Map.unions [idpAppAuthorizeExtraParams, toQueryParam req]
     in TL.fromStrict $
          T.decodeUtf8 $
            serializeURIRef' $
              appendQueryParams allParams $
                idpAuthorizeEndpoint idp

instance HasTokenRequest 'AuthorizationCode where
  -- \| https://www.rfc-editor.org/rfc/rfc6749#section-4.1.3
  data TokenRequest 'AuthorizationCode = AuthorizationCodeTokenRequest
    { code :: ExchangeToken
    , clientId :: ClientId
    , grantType :: GrantTypeValue
    , redirectUri :: RedirectUri
    }
  type WithExchangeToken 'AuthorizationCode a = ExchangeToken -> a

  mkTokenRequest ::
    IdpApplication 'AuthorizationCode i ->
    ExchangeToken ->
    TokenRequest 'AuthorizationCode
  mkTokenRequest AuthorizationCodeIdpApplication {..} authCode =
    AuthorizationCodeTokenRequest
      { code = authCode
      , clientId = idpAppClientId
      , grantType = GTAuthorizationCode
      , redirectUri = RedirectUri idpAppRedirectUri
      }
  conduitTokenRequest ::
    forall m i.
    (MonadIO m) =>
    IdpApplication 'AuthorizationCode i ->
    Manager ->
    ExchangeToken ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitTokenRequest idpAppConfig@AuthorizationCodeIdpApplication {..} mgr exchangeToken =
    let req = mkTokenRequest idpAppConfig exchangeToken
        key = toOAuth2Key idpAppClientId idpAppClientSecret
        body =
          mapsToParams
            [ toQueryParam req
            , toQueryParam
                ( if idpAppTokenRequestAuthenticationMethod == ClientSecretPost
                    then Just idpAppClientSecret
                    else Nothing
                )
            ]
     in doJSONPostRequest mgr key (idpTokenEndpoint idp) body

instance HasPkceAuthorizeRequest 'AuthorizationCode where
  mkPkceAuthorizeRequest :: MonadIO m => IdpApplication 'AuthorizationCode i -> m (Text, CodeVerifier)
  mkPkceAuthorizeRequest idpAppConfig@AuthorizationCodeIdpApplication {..} = do
    PkceRequestParam {..} <- mkPkceParam
    let req = mkAuthorizeRequestParameter idpAppConfig
    let allParams =
          mapsToParams
            [ idpAppAuthorizeExtraParams
            , toQueryParam req
            , toQueryParam codeChallenge
            , toQueryParam codeChallengeMethod
            ]

    let url =
          TL.fromStrict $
            T.decodeUtf8 $
              serializeURIRef' $
                appendQueryParams allParams $
                  idpAuthorizeEndpoint idp
    pure (url, codeVerifier)

instance HasPkceTokenRequest 'AuthorizationCode where
  conduitPkceTokenRequest ::
    MonadIO m =>
    IdpApplication 'AuthorizationCode i ->
    Manager ->
    (ExchangeToken, CodeVerifier) ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitPkceTokenRequest idpAppConfig@AuthorizationCodeIdpApplication {..} mgr (exchangeToken, codeVerifier) =
    let req = mkTokenRequest idpAppConfig exchangeToken
        key = toOAuth2Key idpAppClientId idpAppClientSecret
        body =
          mapsToParams
            [ toQueryParam req
            , toQueryParam codeVerifier
            , toQueryParam (if idpAppTokenRequestAuthenticationMethod == ClientSecretPost then Just idpAppClientSecret else Nothing)
            ]
     in doJSONPostRequest mgr key (idpTokenEndpoint idp) body

instance HasRefreshTokenRequest 'AuthorizationCode where
  data RefreshTokenRequest 'AuthorizationCode = AuthorizationCodeTokenRefreshRequest
    { refreshToken :: OAuth2.RefreshToken
    , grantType :: GrantTypeValue
    , scope :: Set Scope
    }

  mkRefreshTokenRequest :: IdpApplication 'AuthorizationCode i -> OAuth2.RefreshToken -> RefreshTokenRequest 'AuthorizationCode
  mkRefreshTokenRequest AuthorizationCodeIdpApplication {..} rt =
    AuthorizationCodeTokenRefreshRequest
      { scope = idpAppScope
      , grantType = GTRefreshToken
      , refreshToken = rt
      }
  conduitRefreshTokenRequest ::
    (MonadIO m) =>
    IdpApplication 'AuthorizationCode i ->
    Manager ->
    OAuth2.RefreshToken ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitRefreshTokenRequest idpAppConfig@AuthorizationCodeIdpApplication {..} mgr rt =
    let req = mkRefreshTokenRequest idpAppConfig rt
        key = toOAuth2Key idpAppClientId idpAppClientSecret
        body =
          mapsToParams
            [ toQueryParam req
            , toQueryParam (if idpAppTokenRequestAuthenticationMethod == ClientSecretPost then Just idpAppClientSecret else Nothing)
            ]
     in doJSONPostRequest mgr key (idpTokenEndpoint idp) body

instance HasUserInfoRequest 'AuthorizationCode where
  conduitUserInfoRequest ::
    FromJSON (IdpUserInfo i) =>
    IdpApplication 'AuthorizationCode i ->
    Manager ->
    AccessToken ->
    ExceptT BSL.ByteString IO (IdpUserInfo i)
  conduitUserInfoRequest AuthorizationCodeIdpApplication {..} mgr at = do
    idpFetchUserInfo idp mgr at (idpUserInfoEndpoint idp)

instance ToQueryParam (AuthorizationRequest 'AuthorizationCode) where
  toQueryParam :: AuthorizationRequest 'AuthorizationCode -> Map Text Text
  toQueryParam req@AuthorizationCodeAuthorizationRequest {..} =
    Map.unions
      [ toResponseTypeParam req
      , toQueryParam scope
      , toQueryParam clientId
      , toQueryParam state
      , toQueryParam redirectUri
      ]

instance ToQueryParam (TokenRequest 'AuthorizationCode) where
  toQueryParam :: TokenRequest 'AuthorizationCode -> Map Text Text
  toQueryParam AuthorizationCodeTokenRequest {..} =
    Map.unions
      [ toQueryParam grantType
      , toQueryParam code
      , toQueryParam redirectUri
      ]

instance ToQueryParam (RefreshTokenRequest 'AuthorizationCode) where
  toQueryParam :: RefreshTokenRequest 'AuthorizationCode -> Map Text Text
  toQueryParam AuthorizationCodeTokenRefreshRequest {..} =
    Map.unions
      [ toQueryParam grantType
      , toQueryParam scope
      , toQueryParam refreshToken
      ]

-------------------------------------------------------------------------------

-- * JWTBearer

-------------------------------------------------------------------------------

-- | An Application that supports "Authorization code" flow
data instance IdpApplication 'JwtBearer i = JwtBearerIdpApplication
  { idpAppName :: Text
  , idpAppJwt :: BS.ByteString
  , idp :: Idp i
  }

instance HasTokenRequest 'JwtBearer where
  data TokenRequest 'JwtBearer = JwtBearerTokenRequest
    { grantType :: GrantTypeValue
    , assertion :: BS.ByteString
    }
  type WithExchangeToken 'JwtBearer a = a

  mkTokenRequest ::
    IdpApplication 'JwtBearer i ->
    TokenRequest 'JwtBearer
  mkTokenRequest JwtBearerIdpApplication {..} =
    JwtBearerTokenRequest
      { grantType = GTJwtBearer
      , assertion = idpAppJwt
      }

  conduitTokenRequest ::
    forall m i.
    (MonadIO m) =>
    IdpApplication 'JwtBearer i ->
    Manager ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitTokenRequest idpAppConfig@JwtBearerIdpApplication {..} mgr = do
    resp <- ExceptT . liftIO $ do
      let tokenReq = mkTokenRequest idpAppConfig
      let body = mapsToParams [toQueryParam tokenReq]
      req <- uriToRequest (idpTokenEndpoint idp)
      handleOAuth2TokenResponse <$> httpLbs (urlEncodedBody body (addDefaultRequestHeaders req)) mgr
    case parseResponseFlexible resp of
      Right obj -> return obj
      Left e -> throwE e

instance ToQueryParam (TokenRequest 'JwtBearer) where
  toQueryParam :: TokenRequest 'JwtBearer -> Map Text Text
  toQueryParam JwtBearerTokenRequest {..} =
    Map.unions
      [ toQueryParam grantType
      , Map.fromList [("assertion", bs8ToLazyText assertion)]
      ]

instance HasUserInfoRequest 'JwtBearer where
  conduitUserInfoRequest  JwtBearerIdpApplication {..} mgr at = do
    idpFetchUserInfo idp mgr at (idpUserInfoEndpoint idp)

-------------------------------------------------------------------------------

-- * Password flow

-------------------------------------------------------------------------------

-- https://www.rfc-editor.org/rfc/rfc6749#section-4.3.1
-- 4.3.1.  Authorization Request and Response (Password grant type)
-- The method through which the client obtains the resource owner
-- credentials is beyond the scope of this specification.  The client
-- MUST discard the credentials once an access token has been obtained.
--
-- Hence no AuhorizationRequest instance

data instance IdpApplication 'ResourceOwnerPassword i = ResourceOwnerPasswordIDPApplication
  { idpAppClientId :: ClientId
  , idpAppClientSecret :: ClientSecret
  , idpAppName :: Text
  , idpAppScope :: Set Scope
  , idpAppUserName :: Username
  , idpAppPassword :: Password
  , idpAppTokenRequestExtraParams :: Map Text Text
  -- ^ Any parameter that required by your Idp and not mentioned in the OAuth2 spec
  , idp :: Idp i
  }

instance HasIdpAppName 'ResourceOwnerPassword where
  getIdpAppName :: IdpApplication 'ResourceOwnerPassword i -> Text
  getIdpAppName ResourceOwnerPasswordIDPApplication {..} = idpAppName

instance HasUserInfoRequest 'ResourceOwnerPassword where
  conduitUserInfoRequest ResourceOwnerPasswordIDPApplication {..} mgr at = do
    idpFetchUserInfo idp mgr at (idpUserInfoEndpoint idp)

instance HasTokenRequest 'ResourceOwnerPassword where
  -- \| https://www.rfc-editor.org/rfc/rfc6749#section-4.3.2
  data TokenRequest 'ResourceOwnerPassword = PasswordTokenRequest
    { scope :: Set Scope
    , username :: Username
    , password :: Password
    , grantType :: GrantTypeValue
    }
  type WithExchangeToken 'ResourceOwnerPassword a = a

  mkTokenRequest :: IdpApplication 'ResourceOwnerPassword i -> TokenRequest 'ResourceOwnerPassword
  mkTokenRequest ResourceOwnerPasswordIDPApplication {..} =
    PasswordTokenRequest
      { username = idpAppUserName
      , password = idpAppPassword
      , grantType = GTPassword
      , scope = idpAppScope
      }

  conduitTokenRequest ::
    (MonadIO m) =>
    IdpApplication 'ResourceOwnerPassword i ->
    Manager ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitTokenRequest idpAppConfig@ResourceOwnerPasswordIDPApplication {..} mgr =
    let req = mkTokenRequest idpAppConfig
        key = toOAuth2Key idpAppClientId idpAppClientSecret
        body = mapsToParams [idpAppTokenRequestExtraParams, toQueryParam req]
     in doJSONPostRequest mgr key (idpTokenEndpoint idp) body

-- | TODO: TBD
instance HasRefreshTokenRequest 'ResourceOwnerPassword where
  data RefreshTokenRequest 'ResourceOwnerPassword = PasswordRefreshTokenRequest

  mkRefreshTokenRequest ::
    IdpApplication 'ResourceOwnerPassword i ->
    OAuth2.RefreshToken ->
    RefreshTokenRequest 'ResourceOwnerPassword
  mkRefreshTokenRequest = undefined

  conduitRefreshTokenRequest ::
    MonadIO m =>
    IdpApplication 'ResourceOwnerPassword i ->
    Manager ->
    OAuth2.RefreshToken ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitRefreshTokenRequest = undefined

instance ToQueryParam (TokenRequest 'ResourceOwnerPassword) where
  toQueryParam :: TokenRequest 'ResourceOwnerPassword -> Map Text Text
  toQueryParam PasswordTokenRequest {..} =
    Map.unions
      [ toQueryParam grantType
      , toQueryParam scope
      , toQueryParam username
      , toQueryParam password
      ]

-------------------------------------------------------------------------------

-- * Client Credentials flow

-------------------------------------------------------------------------------

-- https://www.rfc-editor.org/rfc/rfc6749#section-4.4.1
-- 4.4.1.  Authorization Request and Response (Client Credentials grant type)
-- Since the client authentication is used as the authorization grant,
-- no additional authorization request is needed.
--
-- Hence no AuhorizationRequest instance

data instance IdpApplication 'ClientCredentials i = ClientCredentialsIDPApplication
  { idpAppClientId :: ClientId
  , idpAppClientSecret :: ClientSecret
  , idpAppName :: Text
  , idpAppScope :: Set Scope
  , idpAppTokenRequestExtraParams :: Map Text Text
  -- ^ Any parameter that required by your Idp and not mentioned in the OAuth2 spec
  , idp :: Idp i
  }

instance HasIdpAppName 'ClientCredentials where
  getIdpAppName :: IdpApplication 'ClientCredentials i -> Text
  getIdpAppName ClientCredentialsIDPApplication {..} = idpAppName

instance HasTokenRequest 'ClientCredentials where
  -- \| https://www.rfc-editor.org/rfc/rfc6749#section-4.4.2
  data TokenRequest 'ClientCredentials = ClientCredentialsTokenRequest
    { scope :: Set Scope
    , grantType :: GrantTypeValue
    }

  type WithExchangeToken 'ClientCredentials a = a

  mkTokenRequest :: IdpApplication 'ClientCredentials i -> TokenRequest 'ClientCredentials
  mkTokenRequest ClientCredentialsIDPApplication {..} =
    ClientCredentialsTokenRequest
      { scope = idpAppScope
      , grantType = GTClientCredentials
      }

  conduitTokenRequest ::
    (MonadIO m) =>
    IdpApplication 'ClientCredentials i ->
    Manager ->
    ExceptT (OAuth2Error TR.Errors) m OAuth2Token
  conduitTokenRequest idpAppConfig@ClientCredentialsIDPApplication {..} mgr =
    let req = mkTokenRequest idpAppConfig
        key =
          toOAuth2Key
            idpAppClientId
            idpAppClientSecret
        body =
          mapsToParams
            [ idpAppTokenRequestExtraParams
            , toQueryParam req
            ]
     in doJSONPostRequest mgr key (idpTokenEndpoint idp) body

instance ToQueryParam (TokenRequest 'ClientCredentials) where
  toQueryParam :: TokenRequest 'ClientCredentials -> Map Text Text
  toQueryParam ClientCredentialsTokenRequest {..} =
    Map.unions
      [ toQueryParam grantType
      , toQueryParam scope
      ]
