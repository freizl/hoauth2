{-# LANGUAGE QuasiQuotes #-}

-- | [Google build oauth2 web server application](https://developers.google.com/identity/protocols/oauth2/web-server)
module Network.OAuth2.Provider.Google where

import Crypto.PubKey.RSA.Types
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString.Contrib
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Time
import GHC.Generics
import Jose.Jwa
import Jose.Jws
import Jose.Jwt
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import OpenSSL.EVP.PKey (toKeyPair)
import OpenSSL.PEM (
  PemPasswordSupply (PwNone),
  readPrivateKey,
 )
import OpenSSL.RSA
import URI.ByteString.QQ

{-
To test at google playground, set redirect uri to "https://developers.google.com/oauthplayground"
-}

type instance IdpUserInfo Google = GoogleUser

-- * Authorization Code flow

sampleGoogleAuthorizationCodeApp :: AuthorizationCodeApplication
sampleGoogleAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acName = "sample-google-authorization-code-app"
    , acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["https://www.googleapis.com/auth/userinfo.email", "https://www.googleapis.com/auth/userinfo.profile"]
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost|]
    , acAuthorizeRequestExtraParams = Map.empty
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

-- * Service Account

sampleServiceAccountApp :: Jwt -> JwtBearerApplication
sampleServiceAccountApp jwt =
  JwtBearerApplication
    { jbName = "sample-google-service-account-app"
    , jbJwtAssertion = unJwt jwt
    }

-- | Service account key (in JSON format) that download from google
data GoogleServiceAccountKey = GoogleServiceAccountKey
  { privateKey :: String
  , clientEmail :: Text
  , projectId :: Text
  , privateKeyId :: Text
  , clientId :: Text
  , authUri :: Text
  , tokenUri :: Text
  , authProviderX509CertUrl :: Text
  , clientX509CertUrl :: Text
  }
  deriving (Generic)

instance FromJSON GoogleServiceAccountKey where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

-- * Service Account

mkJwt ::
  PrivateKey ->
  -- | Private key
  Text ->
  -- | Issuer
  Maybe Text ->
  -- | impersonate user
  Set.Set Scope ->
  -- | Scope
  Idp Google ->
  IO (Either String Jwt)
mkJwt privateKey iss muser scopes idp = do
  now <- getCurrentTime
  let payload =
        bsToStrict $
          Aeson.encode $
            Aeson.object $
              [ "iss" .= iss
              , "scope" .= T.intercalate " " (map (TL.toStrict . unScope) $ Set.toList scopes)
              , "aud" .= idpTokenEndpoint idp
              , "exp" .= tToSeconds (addUTCTime (secondsToNominalDiffTime 300) now) -- 5 minutes expiration time
              , "iat" .= tToSeconds now
              ]
                ++ maybe [] (\a -> ["sub" .= a]) muser
  first show <$> rsaEncode RS256 privateKey payload
  where
    tToSeconds = formatTime defaultTimeLocale "%s"

-- | Read private RSA Key in PEM format
readPemRsaKey ::
  -- | PEM content
  String ->
  IO (Either String PrivateKey)
readPemRsaKey pemStr = do
  somePair <- readPrivateKey pemStr PwNone
  pure $ case (toKeyPair somePair :: Maybe RSAKeyPair) of
    Just k ->
      Right $
        PrivateKey
          { private_pub =
              PublicKey
                { public_size = rsaSize k
                , public_n = rsaN k
                , public_e = rsaE k
                }
          , private_d = rsaD k
          , private_p = rsaP k
          , private_q = rsaQ k
          , private_dP = fromMaybe 0 (rsaDMP1 k)
          , private_dQ = fromMaybe 0 (rsaDMQ1 k)
          , private_qinv = fromMaybe 0 (rsaIQMP k)
          }
    Nothing -> Left "unable to parse PEM to RSA key"

-- * IDP

defaultGoogleIdp :: Idp Google
defaultGoogleIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Google)
    , idpAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|]
    , idpTokenEndpoint = [uri|https://oauth2.googleapis.com/token|]
    , idpUserInfoEndpoint = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
    , idpDeviceAuthorizationEndpoint = Just [uri|https://oauth2.googleapis.com/device/code|]
    }

-- requires scope "https://www.googleapis.com/auth/userinfo.profile" to obtain "name".
-- requires scopes "https://www.googleapis.com/auth/userinfo.email" to obtain "email".
data GoogleUser = GoogleUser
  { name :: Text
  , id :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON GoogleUser
