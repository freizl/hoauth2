{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Idp where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
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
import Network.OAuth2.Provider.Utils
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

createAuthorizationApps :: MonadIO m => (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ExceptT Text m [DemoAuthorizationApp]
createAuthorizationApps (myAuth0Idp, myOktaIdp) = do
  configParams <- readEnvFile
  let initIdpAppConfig :: IdpApplication 'AuthorizationCode i -> IdpApplication 'AuthorizationCode i
      initIdpAppConfig idpAppConfig@AuthorizationCodeIdpApplication {..} =
        case Aeson.lookup (Aeson.fromString $ TL.unpack $ TL.toLower $ getIdpAppName idpAppConfig) configParams of
          Nothing -> idpAppConfig
          Just config ->
            idpAppConfig
              { idpAppClientId = ClientId $ Env.clientId config
              , idpAppClientSecret = ClientSecret $ Env.clientSecret config
              , idpAppRedirectUri = defaultOAuth2RedirectUri
              , idpAppScope = Set.unions [idpAppScope, Set.map Scope (Set.fromList (fromMaybe [] (Env.scopes config)))]
              , idpAppAuthorizeState = AuthorizeState (idpAppName <> ".hoauth2-demo-app-123")
              }
  pure
    [ DemoAuthorizationApp (initIdpAppConfig IAzureAD.defaultAzureADApp)
    , DemoAuthorizationApp (initIdpAppConfig (IAuth0.defaultAuth0App myAuth0Idp))
    , DemoAuthorizationApp (initIdpAppConfig IFacebook.defaultFacebookApp)
    , DemoAuthorizationApp (initIdpAppConfig IFitbit.defaultFitbitApp)
    , DemoAuthorizationApp (initIdpAppConfig IGithub.defaultGithubApp)
    , DemoAuthorizationApp (initIdpAppConfig IGoogle.defaultGoogleApp)
    , DemoAuthorizationApp (initIdpAppConfig ILinkedin.defaultLinkedinApp)
    , DemoAuthorizationApp (initIdpAppConfig (IOkta.defaultOktaApp myOktaIdp))
    , DemoAuthorizationApp (initIdpAppConfig ITwitter.defaultTwitterApp)
    , DemoAuthorizationApp (initIdpAppConfig ISlack.defaultSlackApp)
    , DemoAuthorizationApp (initIdpAppConfig IWeibo.defaultWeiboApp)
    , DemoAuthorizationApp (initIdpAppConfig IZOHO.defaultZohoApp)
    , DemoAuthorizationApp (initIdpAppConfig IStackExchange.defaultStackExchangeApp)
    ]

googleServiceAccountApp ::
  ExceptT
    Text
    IO
    (IdpApplication 'JwtBearer IGoogle.Google)
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
  pure $ IGoogle.defaultServiceAccountApp jwt

oktaPasswordGrantApp :: Idp IOkta.Okta -> IdpApplication 'ResourceOwnerPassword IOkta.Okta
oktaPasswordGrantApp i =
  ResourceOwnerPasswordIDPApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppName = "okta-demo-password-grant-app"
    , idpAppScope = Set.fromList ["openid", "profile"]
    , idpAppUserName = ""
    , idpAppPassword = ""
    , idpAppTokenRequestExtraParams = Map.empty
    , idp = i
    }

-- Base on the document, it works well with custom Athourization Server
-- https://developer.okta.com/docs/guides/implement-grant-type/clientcreds/main/#client-credentials-flow
--
-- With Org AS, got this error
-- Client Credentials requests to the Org Authorization Server must use the private_key_jwt token_endpoint_auth_method
--
oktaClientCredentialsGrantApp :: Idp IOkta.Okta -> IO (IdpApplication 'ClientCredentials IOkta.Okta)
oktaClientCredentialsGrantApp i = do
  let clientId = "0oa9mbklxn2Ac0oJ24x7"
  keyJsonStr <- BS.readFile ".okta-key.json"
  case Aeson.eitherDecodeStrict keyJsonStr of
    Right jwk -> do
      ejwt <- IOkta.mkOktaClientCredentialAppJwt jwk clientId i
      case ejwt of
        Right jwt ->
          pure
            ClientCredentialsIDPApplication
              { idpAppClientId = clientId
              , idpAppClientSecret = ClientSecret (TL.decodeUtf8 $ bsFromStrict $ unJwt jwt)
              , idpAppTokenRequestAuthenticationMethod = ClientAssertionJwt
              , idpAppName = "okta-demo-cc-grant-jwt-app"
              , -- , idpAppScope = Set.fromList ["hw-test"]
                idpAppScope = Set.fromList ["okta.users.read"]
              , idpAppTokenRequestExtraParams = Map.empty
              , idp = i
              }
        Left e -> Prelude.error e
    Left e -> Prelude.error e

-- | https://auth0.com/docs/api/authentication#resource-owner-password
auth0PasswordGrantApp :: Idp IAuth0.Auth0 -> IdpApplication 'ResourceOwnerPassword IAuth0.Auth0
auth0PasswordGrantApp i =
  ResourceOwnerPasswordIDPApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppName = "auth0-demo-password-grant-app"
    , idpAppScope = Set.fromList ["openid", "profile", "email"]
    , idpAppUserName = "test"
    , idpAppPassword = ""
    , idpAppTokenRequestExtraParams = Map.empty
    , idp = i
    }

-- | https://auth0.com/docs/api/authentication#client-credentials-flow
auth0ClientCredentialsGrantApp :: Idp IAuth0.Auth0 -> IdpApplication 'ClientCredentials IAuth0.Auth0
auth0ClientCredentialsGrantApp i =
  ClientCredentialsIDPApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppTokenRequestAuthenticationMethod = ClientSecretPost
    , idpAppName = "auth0-demo-cc-grant-app"
    , idpAppScope = Set.fromList ["read:users"]
    , idpAppTokenRequestExtraParams = Map.fromList [("audience ", "https://freizl.auth0.com/api/v2/")]
    , idp = i
    }

isSupportPkce :: forall a i. ( 'AuthorizationCode ~ a) => IdpApplication a i -> Bool
isSupportPkce AuthorizationCodeIdpApplication {..} =
  let hostStr = idpAuthorizeEndpoint idp ^. (authorityL . _Just . authorityHostL . hostBSL)
   in any (`BS.isInfixOf` hostStr) ["auth0.com", "okta.com", "google.com", "twitter.com"]

instance HasDemoLoginUser IAuth0.Auth0 where
  toLoginUser :: IAuth0.Auth0User -> DemoLoginUser
  toLoginUser IAuth0.Auth0User {..} = DemoLoginUser {loginUserName = name}

instance HasDemoLoginUser IGoogle.Google where
  toLoginUser :: IGoogle.GoogleUser -> DemoLoginUser
  toLoginUser IGoogle.GoogleUser {..} = DemoLoginUser {loginUserName = name}

instance HasDemoLoginUser IZOHO.ZOHO where
  toLoginUser resp =
    let us = IZOHO.users resp
     in case us of
          [] -> DemoLoginUser {loginUserName = "ZOHO: no user found"}
          (a : _) -> DemoLoginUser {loginUserName = IZOHO.fullName a}

instance HasDemoLoginUser IAzureAD.AzureAD where
  toLoginUser :: IAzureAD.AzureADUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IAzureAD.mail ouser}

instance HasDemoLoginUser IWeibo.Weibo where
  toLoginUser :: IWeibo.WeiboUID -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = TL.pack $ show $ IWeibo.uid ouser}

instance HasDemoLoginUser IDropbox.Dropbox where
  toLoginUser :: IDropbox.DropboxUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IDropbox.displayName $ IDropbox.name ouser}

instance HasDemoLoginUser IFacebook.Facebook where
  toLoginUser :: IFacebook.FacebookUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IFacebook.name ouser}

instance HasDemoLoginUser IFitbit.Fitbit where
  toLoginUser :: IFitbit.FitbitUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IFitbit.userName ouser}

instance HasDemoLoginUser IGithub.Github where
  toLoginUser :: IGithub.GithubUser -> DemoLoginUser
  toLoginUser guser = DemoLoginUser {loginUserName = IGithub.name guser}

instance HasDemoLoginUser ILinkedin.Linkedin where
  toLoginUser :: ILinkedin.LinkedinUser -> DemoLoginUser
  toLoginUser ILinkedin.LinkedinUser {..} =
    DemoLoginUser
      { loginUserName = localizedFirstName <> " " <> localizedLastName
      }

instance HasDemoLoginUser ITwitter.Twitter where
  toLoginUser :: ITwitter.TwitterUserResp -> DemoLoginUser
  toLoginUser ITwitter.TwitterUserResp {..} = DemoLoginUser {loginUserName = ITwitter.name twitterUserRespData}

instance HasDemoLoginUser IOkta.Okta where
  toLoginUser :: IOkta.OktaUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = IOkta.name ouser}

instance HasDemoLoginUser ISlack.Slack where
  toLoginUser :: ISlack.SlackUser -> DemoLoginUser
  toLoginUser ouser = DemoLoginUser {loginUserName = ISlack.name ouser}

instance HasDemoLoginUser IStackExchange.StackExchange where
  toLoginUser :: IStackExchange.StackExchangeResp -> DemoLoginUser
  toLoginUser IStackExchange.StackExchangeResp {..} =
    case items of
      [] -> DemoLoginUser {loginUserName = TL.pack "Cannot find stackexchange user"}
      (user : _) -> DemoLoginUser {loginUserName = IStackExchange.displayName user}

-- TODO: use Paths_ module for better to find the file?
envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: MonadIO m => ExceptT Text m Env.EnvConfig
readEnvFile = liftIO $ do
  envFileE <- doesFileExist envFilePath
  if envFileE
    then do
      putStrLn "Found .env.json"
      fileContent <- BS.readFile envFilePath
      case Aeson.eitherDecodeStrict fileContent of
        Left err -> print err >> return Aeson.empty
        Right ec -> return ec
    else return Aeson.empty

initIdps :: MonadIO m => CacheStore -> (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ExceptT Text m ()
initIdps c is = do
  idps <- createAuthorizationApps is
  mapM mkDemoAppEnv idps >>= mapM_ (upsertDemoAppEnv c)

mkDemoAppEnv :: MonadIO m => DemoAuthorizationApp -> ExceptT Text m DemoAppEnv
mkDemoAppEnv ia@(DemoAuthorizationApp idpAppConfig) = do
  re <-
    if isSupportPkce idpAppConfig
      then fmap (second Just) (mkPkceAuthorizeRequest idpAppConfig)
      else pure (mkAuthorizeRequest idpAppConfig, Nothing)
  pure $ DemoAppEnv ia (def {authorizeAbsUri = fst re, authorizePkceCodeVerifier = snd re})
