{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

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

loadCredentialFromConfig ::
  MonadIO m =>
  Text ->
  -- | Idp Application name
  ExceptT Text m (Maybe (ClientId, ClientSecret, Set.Set Scope))
loadCredentialFromConfig idpAppName = do
  configParams <- readEnvFile
  let mConfigs = Aeson.lookup (Aeson.fromString $ TL.unpack $ TL.toLower idpAppName) configParams
  pure (fmap toResult mConfigs)
  where
    toResult :: Env.EnvConfigAuthParams -> (ClientId, ClientSecret, Set.Set Scope)
    toResult env =
      ( ClientId $ Env.clientId env
      , ClientSecret $ Env.clientSecret env
      , Set.map Scope (Set.fromList (fromMaybe [] (Env.scopes env)))
      )

loadCredentialFromConfig2 ::
  MonadIO m =>
  Text ->
  ExceptT Text m (Maybe Env.EnvConfigAuthParams)
loadCredentialFromConfig2 idpAppName = do
  configParams <- readEnvFile
  let mConfigs = Aeson.lookup (Aeson.fromString $ TL.unpack $ TL.toLower idpAppName) configParams
  pure mConfigs

createAuthorizationApps ::
  MonadIO m =>
  (Idp IAuth0.Auth0, Idp IOkta.Okta) ->
  ExceptT Text m [DemoAuthorizationApp]
createAuthorizationApps (myAuth0Idp, myOktaIdp) = do
  sequence
    [ initIdpAppConfig IAzureAD.defaultAzureADIdp IAzureAD.sampleAzureADAuthorizationCodeApp
    , initIdpAppConfig myAuth0Idp IAuth0.sampleAuth0AuthorizationCodeApp
    , initIdpAppConfig IFacebook.defaultFacebookIdp IFacebook.sampleFacebookAuthorizationCodeApp
    , initIdpAppConfig IFitbit.defaultFitbitIdp IFitbit.sampleFitbitAuthorizationCodeApp
    , initIdpAppConfig IGithub.defaultGithubIdp IGithub.sampleGithubAuthorizationCodeApp
    , initIdpAppConfig IDropbox.defaultDropboxIdp IDropbox.sampleDropboxAuthorizationCodeApp
    , initIdpAppConfig IGoogle.defaultGoogleIdp IGoogle.sampleGoogleAuthorizationCodeApp
    , initIdpAppConfig ILinkedin.defaultLinkedinIdp ILinkedin.sampleLinkedinAuthorizationCodeApp
    , initIdpAppConfig myOktaIdp IOkta.sampleOktaAuthorizationCodeApp
    , initIdpAppConfig ITwitter.defaultTwitterIdp ITwitter.sampleTwitterAuthorizationCodeApp
    , initIdpAppConfig ISlack.defaultSlackIdp ISlack.sampleSlackAuthorizationCodeApp
    , initIdpAppConfig IWeibo.defaultWeiboIdp IWeibo.sampleWeiboAuthorizationCodeApp
    , initIdpAppConfig IZOHO.defaultZohoIdp IZOHO.sampleZohoAuthorizationCodeApp
    , initIdpAppConfig IStackExchange.defaultStackExchangeIdp IStackExchange.sampleStackExchangeAuthorizationCodeApp
    ]
  where
    initIdpAppConfig :: (MonadIO m, HasDemoLoginUser i, Aeson.FromJSON (IdpUserInfo i)) => Idp i -> AuthorizationCodeApplication -> ExceptT Text m DemoAuthorizationApp
    initIdpAppConfig i idpApplication = do
      resp <- loadCredentialFromConfig (getIdpAppName idpApplication)
      let newApp = case resp of
            Nothing -> idpApplication
            Just (cid, csecret, cscopes) ->
              idpApplication
                { acClientId = cid
                , acClientSecret = csecret
                , acRedirectUri = defaultOAuth2RedirectUri
                , acScope = Set.unions [acScope idpApplication, cscopes]
                , acAuthorizeState = AuthorizeState (acName idpApplication <> ".hoauth2-demo-app-123")
                }
      pure (DemoAuthorizationApp $ IdpApplication {idp = i, application = newApp})

googleServiceAccountApp :: ExceptT Text IO (IdpApplication IGoogle.Google JwtBearerApplication)
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
  pure $
    IdpApplication
      { idp = IGoogle.defaultGoogleIdp
      , application = IGoogle.sampleServiceAccountApp jwt
      }

-- | https://auth0.com/docs/api/authentication#resource-owner-password
createResourceOwnerPasswordApp ::
  Idp i ->
  Text ->
  ExceptT Text IO (IdpApplication i ResourceOwnerPasswordApplication)
createResourceOwnerPasswordApp i idpName = do
  let newAppName = "sample-" <> idpName <> "-resource-owner-app"
  let defaultApp =
        ResourceOwnerPasswordApplication
          { ropClientId = ""
          , ropClientSecret = ""
          , ropName = newAppName
          , ropScope = Set.fromList ["openid", "profile", "email"]
          , ropUserName = ""
          , ropPassword = ""
          , ropTokenRequestExtraParams = Map.empty
          }
  resp <- loadCredentialFromConfig2 newAppName
  newApp' <- case resp of
    Nothing -> throwE ("[createResourceOwnerPasswordApp] unable to load config for " <> idpName)
    Just env ->
      case Env.user env of
        Nothing -> throwE ("[createResourceOwnerPasswordApp] unable to load user config for " <> idpName)
        Just userConfig ->
          pure
            defaultApp
              { ropClientId = ClientId (Env.clientId env)
              , ropClientSecret = ClientSecret (Env.clientSecret env)
              , ropUserName = Username (Env.username userConfig)
              , ropPassword = Password (Env.password userConfig)
              }
  pure $
    IdpApplication
      { idp = i
      , application = newApp'
      }

-- | https://auth0.com/docs/api/authentication#client-credentials-flow
createClientCredentialsApp ::
  Idp i ->
  Text ->
  ExceptT Text IO (IdpApplication i ClientCredentialsApplication)
createClientCredentialsApp i idpName = do
  let newAppName = "sample-" <> idpName <> "-client-credentials-app"
  let defaultApp =
        ClientCredentialsApplication
          { ccClientId = ""
          , ccClientSecret = ""
          , ccTokenRequestAuthenticationMethod = ClientSecretPost
          , ccName = ""
          , ccScope = Set.empty
          , ccTokenRequestExtraParams = Map.empty
          }

  resp <- loadCredentialFromConfig newAppName
  newApp <- case idpName of
    "auth0" ->
      pure
        defaultApp
          { ccTokenRequestExtraParams = Map.fromList [("audience ", "https://freizl.auth0.com/api/v2/")]
          }
    -- "okta" -> createOktaClientCredentialsGrantAppJwt i resp
    _ -> pure defaultApp
  let newApp' = case resp of
        Nothing -> newApp
        Just (cid, cc, cs) ->
          newApp
            { ccClientId = cid
            , ccClientSecret = cc
            , ccScope = cs
            , ccName = newAppName
            }
  pure $
    IdpApplication
      { idp = i
      , application = newApp'
      }

-- Base on the document, it works well with both custom Athourization Server and Org As.
-- https://developer.okta.com/docs/guides/implement-grant-type/clientcreds/main/#client-credentials-flow
--
-- But with Org AS, has to use jwt athentication method otherwise got error
-- Client Credentials requests to the Org Authorization Server must use the private_key_jwt token_endpoint_auth_method
--
-- TODO: get error from Okta about parsing assertion error
createOktaClientCredentialsGrantAppJwt ::
  Idp i ->
  Maybe (ClientId, ClientSecret, Set.Set Scope) ->
  ExceptT Text IO ClientCredentialsApplication
createOktaClientCredentialsGrantAppJwt i mresp = do
  clientId <- case mresp of
    Nothing -> throwE "createOktaClientCredentialsGrantApp failed: missing client_id"
    Just (a, _, _) -> pure a
  keyJsonStr <- liftIO $ BS.readFile ".okta-key.json"
  jwk <- except (first TL.pack $ Aeson.eitherDecodeStrict keyJsonStr)
  jwt <- ExceptT $ IOkta.mkOktaClientCredentialAppJwt jwk clientId i
  pure
    ClientCredentialsApplication
      { ccClientId = clientId
      , ccClientSecret = ClientSecret (TL.decodeUtf8 $ bsFromStrict $ unJwt jwt)
      , ccTokenRequestAuthenticationMethod = ClientAssertionJwt
      , ccName = ""
      , ccScope = Set.empty
      , ccTokenRequestExtraParams = Map.empty
      }

createDeviceAuthApp ::
  Idp i ->
  Text ->
  ExceptT Text IO (IdpApplication i DeviceAuthorizationApplication)
createDeviceAuthApp i idpName = do
  let authMethod =
        if "okta" `TL.isInfixOf` idpName
          then Just ClientSecretBasic
          else Nothing
      extraParams =
        if "azuread" `TL.isInfixOf` idpName
          then Map.singleton "tenant" "/common"
          else Map.empty
  let newAppName = "sample-" <> idpName <> "-device-authorization-app"
      newApp =
        DeviceAuthorizationApplication
          { daClientId = ""
          , daClientSecret = ""
          , daName = newAppName
          , daScope = Set.empty
          , daAuthorizationRequestExtraParam = extraParams
          , daAuthorizationRequestAuthenticationMethod = authMethod
          }
  resp <- loadCredentialFromConfig newAppName
  let newApp' = case resp of
        Nothing -> newApp
        Just (cid, cc, cs) ->
          newApp
            { daClientId = cid
            , daClientSecret = cc
            , daScope = cs
            }
  pure $
    IdpApplication
      { idp = i
      , application = newApp'
      }

findIdp ::
  MonadIO m =>
  (Idp IAuth0.Auth0, Idp IOkta.Okta) ->
  Text ->
  ExceptT Text m DemoIdp
findIdp (myAuth0Idp, myOktaIdp) idpName = case idpName of
  "azuread" -> pure (DemoIdp IAzureAD.defaultAzureADIdp)
  "auth0" -> pure (DemoIdp myAuth0Idp)
  "okta" -> pure (DemoIdp myOktaIdp)
  "facebook" -> pure (DemoIdp IFacebook.defaultFacebookIdp)
  "fitbit" -> pure (DemoIdp IFitbit.defaultFitbitIdp)
  "github" -> pure (DemoIdp IGithub.defaultGithubIdp)
  "dropbox" -> pure (DemoIdp IDropbox.defaultDropboxIdp)
  "google" -> pure (DemoIdp IGoogle.defaultGoogleIdp)
  "linkedin" -> pure (DemoIdp ILinkedin.defaultLinkedinIdp)
  "twitter" -> pure (DemoIdp ITwitter.defaultTwitterIdp)
  "slack" -> pure (DemoIdp ISlack.defaultSlackIdp)
  "weibo" -> pure (DemoIdp IWeibo.defaultWeiboIdp)
  "zoho" -> pure (DemoIdp IZOHO.defaultZohoIdp)
  "stackexchange" -> pure (DemoIdp IStackExchange.defaultStackExchangeIdp)
  _ -> throwE ("Unable to find Idp for: " <> idpName)

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

readEnvFile :: MonadIO m => ExceptT Text m Env.EnvConfig
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

initIdps ::
  MonadIO m =>
  CacheStore ->
  (Idp IAuth0.Auth0, Idp IOkta.Okta) ->
  ExceptT Text m ()
initIdps c is = do
  idps <- createAuthorizationApps is
  mapM mkDemoAppEnv idps >>= mapM_ (upsertDemoAppEnv c)

mkDemoAppEnv :: MonadIO m => DemoAuthorizationApp -> ExceptT Text m DemoAppEnv
mkDemoAppEnv ia@(DemoAuthorizationApp idpAppConfig) = do
  re <-
    if isSupportPkce idpAppConfig
      then fmap (second Just) (mkPkceAuthorizeRequest idpAppConfig)
      else pure (mkAuthorizationRequest idpAppConfig, Nothing)
  pure $
    DemoAppEnv
      ia
      ( def
          { authorizeAbsUri = TL.fromStrict $ uriToText (fst re)
          , authorizePkceCodeVerifier = snd re
          }
      )
