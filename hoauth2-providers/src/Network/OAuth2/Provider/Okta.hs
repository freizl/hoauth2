{-# LANGUAGE QuasiQuotes #-}

-- https://developer.okta.com/docs/reference/api/oidc/#request-parameters
-- Okta Org AS doesn't support consent
-- Okta Custom AS does support consent via config (what scope shall prompt consent)

-- | [Okta OIDC & OAuth2 API](https://developer.okta.com/docs/reference/api/oidc/)
module Network.OAuth2.Provider.Okta where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString.Contrib
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy as TL
import Data.Time
import GHC.Generics
import Jose.Jwa
import Jose.Jwk
import Jose.Jws
import Jose.Jwt
import Network.HTTP.Conduit (Manager)
import Network.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Network.OIDC.WellKnown
import URI.ByteString.QQ

sampleOktaAuthorizationCodeApp :: AuthorizationCodeApplication
sampleOktaAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile", "email"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-okta-authorization-code-app"
    , acClientAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequest

mkOktaIdp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @foo.okta.com@
  Text ->
  ExceptT Text m (Idp Okta)
mkOktaIdp domain = do
  OpenIDConfiguration {..} <- fetchWellKnown domain
  pure $
    Idp
      { idpUserInfoEndpoint = userinfoEndpoint
      , idpAuthorizeEndpoint = authorizationEndpoint
      , idpTokenEndpoint = tokenEndpoint
      , idpDeviceAuthorizationEndpoint = Just deviceAuthorizationEndpoint
      }

mkOktaClientCredentialAppJwt ::
  Jwk ->
  ClientId ->
  Idp i ->
  IO (Either Text Jwt)
mkOktaClientCredentialAppJwt jwk cid idp = do
  now <- getCurrentTime
  let cidStr = unClientId cid
  let payload =
        bsToStrict $
          Aeson.encode $
            Aeson.object
              [ "aud" .= idpTokenEndpoint idp
              , "exp" .= tToSeconds (addUTCTime (secondsToNominalDiffTime 300) now) -- 5 minutes expiration time
              , "iat" .= tToSeconds now
              , "iss" .= cidStr
              , "sub" .= cidStr
              ]
  first (TL.pack . show) <$> jwkEncode RS256 jwk (Claims payload)
  where
    tToSeconds = formatTime defaultTimeLocale "%s"

data OktaUser = OktaUser
  { sub :: Text
  , email :: Text
  , preferredUsername :: Text
  , givenName :: Text
  , familyName :: Text
  , emailVerified :: Bool
  }
  deriving (Show, Generic)

instance FromJSON OktaUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
