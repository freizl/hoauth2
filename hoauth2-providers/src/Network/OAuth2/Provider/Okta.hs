{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Network.OAuth2.Provider.Okta where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OIDC.WellKnown
import URI.ByteString.QQ

data Okta = Okta deriving (Eq, Show)

type instance IdpUserInfo Okta = OktaUser

defaultOktaApp :: Idp Okta -> IdpApplication 'AuthorizationCode Okta
defaultOktaApp i =
  AuthorizationCodeIdpApplication
    { idpAppClientId = "",
      idpAppClientSecret = "",
      idpAppScope = Set.fromList ["openid", "profile", "email"],
      idpAppAuthorizeState = "CHANGE_ME",
      idpAppAuthorizeExtraParams = Map.empty,
      idpAppRedirectUri = [uri|http://localhost|],
      idpAppName = "default-okta-App",
      idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
      idp = i
    }

defaultOktaIdp :: Idp Okta
defaultOktaIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Okta),
      idpUserInfoEndpoint = [uri|https://foo.okta.com/oauth2/v1/userinfo|],
      idpAuthorizeEndpoint =
        [uri|https://foo.okta.com/oauth2/v1/authorize|],
      idpTokenEndpoint =
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
        { idpUserInfoEndpoint = userinfoUri,
          idpAuthorizeEndpoint = authorizationUri,
          idpTokenEndpoint = tokenUri
        }
    )

-- https://developer.okta.com/docs/reference/api/oidc/#request-parameters
-- Okta Org AS doesn't support consent
-- Okta Custom AS does support consent via config (what scope shall prompt consent)
data OktaUser = OktaUser
  { name :: Text,
    preferredUsername :: Text
  }
  deriving (Show, Generic)

instance FromJSON OktaUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
