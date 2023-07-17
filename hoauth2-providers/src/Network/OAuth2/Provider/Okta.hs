{-# LANGUAGE QuasiQuotes #-}

-- | [Okta OIDC & OAuth2 API](https://developer.okta.com/docs/reference/api/oidc/)
module Network.OAuth2.Provider.Okta where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString.Contrib
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Time
import GHC.Generics
import Jose.Jwa
import Jose.Jwk
import Jose.Jws
import Jose.Jwt
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OIDC.WellKnown
import URI.ByteString.QQ

data Okta = Okta deriving (Eq, Show)

type instance IdpUserInfo Okta = OktaUser

defaultOktaApp :: AuthorizationCodeApplication
defaultOktaApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["openid", "profile", "email"]
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "default-okta-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

mkOktaIdp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @foo.okta.com@
  Text ->
  ExceptT Text m (Idp Okta)
mkOktaIdp domain = do
  OpenIDConfiguration {..} <- fetchWellKnown domain
  pure $
    Idp
      { idpFetchUserInfo = authGetJSON @(IdpUserInfo Okta)
      , idpUserInfoEndpoint = userinfoEndpoint
      , idpAuthorizeEndpoint = authorizationEndpoint
      , idpTokenEndpoint = tokenEndpoint
      , idpDeviceAuthorizationEndpoint = Just deviceAuthorizationEndpoint
      }

mkOktaClientCredentialAppJwt ::
  Jwk ->
  ClientId ->
  Idp Okta ->
  IO (Either String Jwt)
mkOktaClientCredentialAppJwt jwk cid idp = do
  now <- getCurrentTime
  let cidStr = unClientId cid
  let payload =
        bsToStrict $
          Aeson.encode $
            Aeson.object
              [ "iss" .= cidStr
              , "sub" .= cidStr
              , "aud" .= idpTokenEndpoint idp
              , "exp" .= tToSeconds (addUTCTime (secondsToNominalDiffTime 300) now) -- 5 minutes expiration time
              , "iat" .= tToSeconds now
              ]
  first show <$> jwkEncode RS256 jwk (Claims payload)
  where
    tToSeconds = formatTime defaultTimeLocale "%s"

-- https://developer.okta.com/docs/reference/api/oidc/#request-parameters
-- Okta Org AS doesn't support consent
-- Okta Custom AS does support consent via config (what scope shall prompt consent)
data OktaUser = OktaUser
  { name :: Text
  , preferredUsername :: Text
  }
  deriving (Show, Generic)

instance FromJSON OktaUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
