{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Idp where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Contrib
import Data.Default
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Env qualified
import Jose.Jwt
import Lens.Micro
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Experiment.GrantType.AuthorizationCode (Application (..))
import Network.OAuth2.Experiment.GrantType.AuthorizationCode qualified as AuthorizationCode
import Network.OAuth2.Experiment.GrantType.ClientCredentials qualified as ClientCredentials
import Network.OAuth2.Experiment.GrantType.JwtBearer qualified as JwtBearer
import Network.OAuth2.Experiment.GrantType.ResourceOwnerPassword qualified as ResourceOwnerPassword
import Network.OAuth2.Provider.Auth0 qualified as IAuth0
import Network.OAuth2.Provider.AzureAD qualified as IAzureAD
import Network.OAuth2.Provider.Dropbox qualified as IDropbox
import Network.OAuth2.Provider.Facebook qualified as IFacebook
import Network.OAuth2.Provider.Fitbit qualified as IFitbit
import Network.OAuth2.Provider.Github qualified as IGithub
import Network.OAuth2.Provider.Google qualified as IGoogle
import Network.OAuth2.Provider.Linkedin qualified as ILinkedin
import Network.OAuth2.Provider.Okta qualified as IOkta
import Network.OAuth2.Provider.Slack qualified as ISlack
import Network.OAuth2.Provider.StackExchange qualified as IStackExchange
import Network.OAuth2.Provider.Twitter qualified as ITwitter
import Network.OAuth2.Provider.Weibo qualified as IWeibo
import Network.OAuth2.Provider.ZOHO qualified as IZOHO
import Session
import System.Directory
import Types
import URI.ByteString
import URI.ByteString.QQ (uri)
import Prelude hiding (id)

defaultOAuth2RedirectUri :: URI
defaultOAuth2RedirectUri = [uri|http://localhost:9988/oauth2/callback|]

createAuthorizationApps :: (MonadIO m) => (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ExceptT Text m [DemoAuthorizationApp]
createAuthorizationApps (myAuth0Idp, myOktaIdp) = do
  configParams <- readEnvFile
  let initIdpAppConfig :: Idp i -> AuthorizationCode.Application -> IdpApplication i AuthorizationCode.Application
      initIdpAppConfig i idpAppConfig =
        case Aeson.lookup (Aeson.fromString $ TL.unpack $ TL.toLower $ getIdpAppName idpAppConfig) configParams of
          Nothing -> IdpApplication {idp = i, application = idpAppConfig}
          Just config ->
            let oldApp = idpAppConfig
                newApp =
                  oldApp
                    { acClientId = ClientId $ Env.clientId config
                    , acClientSecret = ClientSecret $ Env.clientSecret config
                    , acRedirectUri = defaultOAuth2RedirectUri
                    , acScope = Set.unions [acScope oldApp, Set.map Scope (Set.fromList (fromMaybe [] (Env.scopes config)))]
                    , acAuthorizeState = AuthorizeState (acName oldApp <> ".hoauth2-demo-app-123")
                    }
             in IdpApplication {idp = i, application = newApp}
  pure
    [ DemoAuthorizationApp (initIdpAppConfig IAzureAD.defaultAzureADIdp IAzureAD.defaultAzureADApp)
    , DemoAuthorizationApp (initIdpAppConfig myAuth0Idp IAuth0.defaultAuth0App)
    , DemoAuthorizationApp (initIdpAppConfig IFacebook.defaultFacebookIdp IFacebook.defaultFacebookApp)
    , DemoAuthorizationApp (initIdpAppConfig IFitbit.defaultFitbitIdp IFitbit.defaultFitbitApp)
    , DemoAuthorizationApp (initIdpAppConfig IGithub.defaultGithubIdp IGithub.defaultGithubApp)
    , DemoAuthorizationApp (initIdpAppConfig IDropbox.defaultDropboxIdp IDropbox.defaultDropboxApp)
    , DemoAuthorizationApp (initIdpAppConfig IGoogle.defaultGoogleIdp IGoogle.defaultGoogleApp)
    , DemoAuthorizationApp (initIdpAppConfig ILinkedin.defaultLinkedinIdp ILinkedin.defaultLinkedinApp)
    , DemoAuthorizationApp (initIdpAppConfig myOktaIdp IOkta.defaultOktaApp)
    , DemoAuthorizationApp (initIdpAppConfig ITwitter.defaultTwitterIdp ITwitter.defaultTwitterApp)
    , DemoAuthorizationApp (initIdpAppConfig ISlack.defaultSlackIdp ISlack.defaultSlackApp)
    , DemoAuthorizationApp (initIdpAppConfig IWeibo.defaultWeiboIdp IWeibo.defaultWeiboApp)
    , DemoAuthorizationApp (initIdpAppConfig IZOHO.defaultZohoIdp IZOHO.defaultZohoApp)
    , DemoAuthorizationApp (initIdpAppConfig IStackExchange.defaultStackExchangeIdp IStackExchange.defaultStackExchangeApp)
    ]

googleServiceAccountApp :: ExceptT Text IO (IdpApplication IGoogle.Google JwtBearer.Application)
googleServiceAccountApp = do
  IGoogle.GoogleServiceAccountKey {..} <- withExceptT TL.pack (ExceptT $ Aeson.eitherDecodeFileStrict ".google-sa.json")
  pkey <- withExceptT TL.pack (ExceptT $ IGoogle.readPemRsaKey privateKey)
  jwt <-
    withExceptT
      TL.pack
      ( ExceptT $
          IGoogle.mkJwt
            pkey
            clientEmail
            Nothing
            ( Set.fromList
                [ "https://www.googleapis.com/auth/userinfo.email"
                , "https://www.googleapis.com/auth/userinfo.profile"
                ]
            )
            IGoogle.defaultGoogleIdp
      )
  pure $ IdpApplication {idp = IGoogle.defaultGoogleIdp, application = IGoogle.defaultServiceAccountApp jwt}

oktaPasswordGrantApp :: Idp IOkta.Okta -> IdpApplication IOkta.Okta ResourceOwnerPassword.Application
oktaPasswordGrantApp i =
  IdpApplication
    { idp = i
    , application =
        ResourceOwnerPassword.Application
          { ropClientId = ""
          , ropClientSecret = ""
          , ropName = "okta-demo-password-grant-app"
          , ropScope = Set.fromList ["openid", "profile"]
          , ropUserName = ""
          , ropPassword = ""
          , ropTokenRequestExtraParams = Map.empty
          }
    }

-- Base on the document, it works well with custom Athourization Server
-- https://developer.okta.com/docs/guides/implement-grant-type/clientcreds/main/#client-credentials-flow
--
-- With Org AS, got this error
-- Client Credentials requests to the Org Authorization Server must use the private_key_jwt token_endpoint_auth_method
--
oktaClientCredentialsGrantApp :: Idp IOkta.Okta -> IO (IdpApplication IOkta.Okta ClientCredentials.Application)
oktaClientCredentialsGrantApp i = do
  let clientId = "0oa9mbklxn2Ac0oJ24x7"
  keyJsonStr <- BS.readFile ".okta-key.json"
  case Aeson.eitherDecodeStrict keyJsonStr of
    Right jwk -> do
      ejwt <- IOkta.mkOktaClientCredentialAppJwt jwk clientId i
      case ejwt of
        Right jwt ->
          pure
            IdpApplication
              { idp = i
              , application =
                  ClientCredentials.Application
                    { ccClientId = clientId
                    , ccClientSecret = ClientSecret (TL.decodeUtf8 $ bsFromStrict $ unJwt jwt)
                    , ccTokenRequestAuthenticationMethod = ClientAssertionJwt
                    , ccName = "okta-demo-cc-grant-jwt-app"
                    , -- , idpAppScope = Set.fromList ["hw-test"]
                      ccScope = Set.fromList ["okta.users.read"]
                    , ccTokenRequestExtraParams = Map.empty
                    }
              }
        Left e -> Prelude.error e
    Left e -> Prelude.error e

-- | https://auth0.com/docs/api/authentication#resource-owner-password
auth0PasswordGrantApp :: Idp IAuth0.Auth0 -> IdpApplication IAuth0.Auth0 ResourceOwnerPassword.Application
auth0PasswordGrantApp i =
  IdpApplication
    { idp = i
    , application =
        ResourceOwnerPassword.Application
          { ropClientId = ""
          , ropClientSecret = ""
          , ropName = "auth0-demo-password-grant-app"
          , ropScope = Set.fromList ["openid", "profile", "email"]
          , ropUserName = "test"
          , ropPassword = ""
          , ropTokenRequestExtraParams = Map.empty
          }
    }

-- | https://auth0.com/docs/api/authentication#client-credentials-flow
auth0ClientCredentialsGrantApp :: Idp IAuth0.Auth0 -> IdpApplication IAuth0.Auth0 ClientCredentials.Application
auth0ClientCredentialsGrantApp i =
  IdpApplication
    { idp = i
    , application =
        ClientCredentials.Application
          { ccClientId = ""
          , ccClientSecret = ""
          , ccTokenRequestAuthenticationMethod = ClientSecretPost
          , ccName = "auth0-demo-cc-grant-app"
          , ccScope = Set.fromList ["read:users"]
          , ccTokenRequestExtraParams = Map.fromList [("audience ", "https://freizl.auth0.com/api/v2/")]
          }
    }

isSupportPkce :: IdpApplication a i -> Bool
isSupportPkce IdpApplication {..} =
  let hostStr = idpAuthorizeEndpoint idp ^. (authorityL . _Just . authorityHostL . hostBSL)
   in any
        (`BS.isInfixOf` hostStr)
        [ "auth0.com"
        , "okta.com"
        , "google.com"
        , "twitter.com"
        ]

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: (MonadIO m) => ExceptT Text m Env.EnvConfig
readEnvFile = liftIO $ do
  pwd <- getCurrentDirectory
  envFileE <- doesFileExist (pwd <> "/" <> envFilePath)
  if envFileE
    then do
      putStrLn "Found .env.json"
      fileContent <- BS.readFile envFilePath
      case Aeson.eitherDecodeStrict fileContent of
        Left err -> print err >> Prelude.error "Unable to parse .env.json"
        Right ec -> return ec
    else return Aeson.empty

initIdps :: (MonadIO m) => CacheStore -> (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ExceptT Text m ()
initIdps c is = do
  idps <- createAuthorizationApps is
  mapM mkDemoAppEnv idps >>= mapM_ (upsertDemoAppEnv c)

mkDemoAppEnv :: (MonadIO m) => DemoAuthorizationApp -> ExceptT Text m DemoAppEnv
mkDemoAppEnv ia@(DemoAuthorizationApp idpAppConfig) = do
  re <-
    if isSupportPkce idpAppConfig
      then fmap (second Just) (mkPkceAuthorizeRequest idpAppConfig)
      else pure (mkAuthorizeRequest idpAppConfig, Nothing)
  pure $ DemoAppEnv ia (def {authorizeAbsUri = fst re, authorizePkceCodeVerifier = snd re})
