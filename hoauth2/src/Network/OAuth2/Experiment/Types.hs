{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Network.OAuth2.Experiment.Types where

-- import Control.Monad.IO.Class (MonadIO (..))
-- import Control.Monad.Trans.Except (ExceptT (..))
-- import Data.Aeson (FromJSON)
-- import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Default (Default (def))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
-- import Network.HTTP.Conduit
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Utils
import URI.ByteString hiding (UserInfo)

-------------------------------------------------------------------------------

-- * Idp App

-------------------------------------------------------------------------------

-- TODO:
-- Maybe worth data type to distinguish authorize and token endpoint
-- as I made mistake at passing to Authorize and Token Request
--
-- NOTE:
-- The 'i' is being PolyKinds. Hence whenever 'Idp i' or 'IdpApplication i a'
-- is used as function parameter, PolyKinds shall be enabled.
--
data Idp (i :: k) = Idp
  { idpUserInfoEndpoint :: URI
  -- ^ Userinfo Endpoint
  , idpAuthorizeEndpoint :: URI
  -- ^ Authorization Endpoint
  , idpTokenEndpoint :: URI
  -- ^ Token Endpoint
  , idpDeviceAuthorizationEndpoint :: Maybe URI
  -- ^ Apparently not all IdP support device code flow
  }

data IdpApplication (i :: k) a = IdpApplication
  { idp :: Idp i
  , application :: a
  }

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
  deriving (Eq, Ord)

instance IsString Scope where
  fromString :: String -> Scope
  fromString = Scope . TL.pack

-------------------------------------------------------------------------------

-- * Grant Type value

-------------------------------------------------------------------------------

-- | Grant type query parameter has association with different GrantType flows but not completely strict.
--
-- e.g. Both AuthorizationCode and ResourceOwnerPassword flow could support refresh token flow.
data GrantTypeValue
  = GTAuthorizationCode
  | GTPassword
  | GTClientCredentials
  | GTRefreshToken
  | GTJwtBearer
  | GTDeviceCode
  deriving (Eq, Show)

-------------------------------------------------------------------------------
--                               Response Type                               --
-------------------------------------------------------------------------------
data ResponseType = Code

-------------------------------------------------------------------------------

-- * Credentials

-------------------------------------------------------------------------------
newtype ClientId = ClientId {unClientId :: Text}
  deriving (Show, Eq, IsString)

-- | Can be either "Client Secret" or JWT base on client authentication method
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
      val GTDeviceCode = "urn:ietf:params:oauth:grant-type:device_code"

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
      toScopeParam :: IsString a => Set Text -> Map a Text
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

instance ToQueryParam ResponseType where
  toQueryParam :: ResponseType -> Map Text Text
  toQueryParam Code = Map.singleton "response_type" "code"

-------------------------------------------------------------------------------
--                                HasOAuth2Key                               --
--                                                                           --
-- Find a way to reuse some methods from old implementation                  --
-- Probably will be removed when Experiment module becomes default           --
-------------------------------------------------------------------------------

class HasOAuth2Key a where
  mkOAuth2Key :: a -> OAuth2
