{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.Okta where

import Data.Bifunctor
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Time
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OIDC.WellKnown
import URI.ByteString.QQ
import Jose.Jwt
import Jose.Jws
import Jose.Jwa
import Jose.Jwk

data Okta = Okta deriving (Eq, Show)

type instance IdpUserInfo Okta = OktaUser

defaultOktaApp :: Idp Okta -> IdpApplication 'AuthorizationCode Okta
defaultOktaApp i =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.fromList ["openid", "profile", "email"]
    , idpAppAuthorizeState = "CHANGE_ME"
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppRedirectUri = [uri|http://localhost|]
    , idpAppName = "default-okta-App"
    , idpAppTokenRequestAuthenticationMethod = ClientSecretBasic
    , idp = i
    }

defaultOktaIdp :: Idp Okta
defaultOktaIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Okta)
    , idpUserInfoEndpoint = [uri|https://foo.okta.com/oauth2/v1/userinfo|]
    , idpAuthorizeEndpoint =
        [uri|https://foo.okta.com/oauth2/v1/authorize|]
    , idpTokenEndpoint =
        [uri|https://foo.okta.com/oauth2/v1/token|]
    }

mkOktaIdp ::
  MonadIO m =>
  -- | Full domain with no http protocol. e.g. @foo.okta.com@
  Text ->
  ExceptT Text m (Idp Okta)
mkOktaIdp domain = do
  OpenIDConfigurationUris {..} <- fetchWellKnownUris domain
  pure
    ( defaultOktaIdp
        { idpUserInfoEndpoint = userinfoUri
        , idpAuthorizeEndpoint = authorizationUri
        , idpTokenEndpoint = tokenUri
        }
    )

mkOktaClientCredentialAppJwt ::
  ByteString ->
  -- | Private key
  ClientId ->
  Idp Okta ->
  IO (Either String Jwt)
mkOktaClientCredentialAppJwt jsonJwk cid idp =
   case Aeson.eitherDecodeStrict jsonJwk of
     Right jwk -> do
        now <- getCurrentTime
        let cidStr = unClientId cid
        let header = Aeson.encode $ Aeson.object
                        [ "alg" .= RS256
                        , "typ" .= ( "JWT" :: Text)
                        ]
        let body = Aeson.encode $ Aeson.object
                        [ "iss" .= cidStr
                        , "sub" .= cidStr
                        , "aud" .= idpTokenEndpoint idp
                        , "exp" .= (addUTCTime ( secondsToNominalDiffTime 300 ) now) -- 5 minutes expiration time
                        , "iat" .= now
                        ]
        let payload = header <> "." <> body
        first show <$> (jwkEncode RS256 jwk (Claims $ BS.toStrict body))
     Left e -> pure (Left e)

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
