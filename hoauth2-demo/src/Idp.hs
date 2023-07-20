{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Idp where

import AppEnv
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Contrib
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Env qualified
import Jose.Jwt
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
import Types
import URI.ByteString
import URI.ByteString.QQ (uri)
import User
import Prelude hiding (id)

defaultOAuth2RedirectUri :: URI
defaultOAuth2RedirectUri = [uri|http://localhost:9988/oauth2/callback|]

createAuthorizationCodeApp ::
  (Aeson.FromJSON (IdpUserInfo i), HasDemoLoginUser i) =>
  TenantBasedIdps ->
  Idp i ->
  IdpName ->
  ExceptT Text IO (IdpApplication i AuthorizationCodeApplication)
createAuthorizationCodeApp oidcIdps idp (IdpName idpName) = do
  let newAppName = "sample-" <> idpName <> "-authorization-code-app"
  newApp <- case findAuthorizationAppByIdp oidcIdps (DemoIdp idp) of
    Just a -> pure a
    Nothing -> throwE ("Unable to create authorization app for idp: " <> idpName)
  Env.OAuthAppSetting {..} <- Env.lookup newAppName
  let newApp' =
        newApp
          { acClientId = clientId
          , acClientSecret = clientSecret
          , acScope = if Set.null scopes then acScope newApp else scopes
          , acRedirectUri = defaultOAuth2RedirectUri
          , acAuthorizeState = AuthorizeState (idpName <> ".hoauth2-demo-app-123")
          }
  pure IdpApplication {idp = idp, application = newApp'}

findAuthorizationAppByIdp ::
  TenantBasedIdps ->
  DemoIdp ->
  Maybe AuthorizationCodeApplication
findAuthorizationAppByIdp (myAuth0Idp, myOktaIdp) idp
  | idp == DemoIdp myAuth0Idp = Just IAuth0.sampleAuth0AuthorizationCodeApp
  | idp == DemoIdp myOktaIdp = Just IOkta.sampleOktaAuthorizationCodeApp
  | idp == DemoIdp IAzureAD.defaultAzureADIdp = Just IAzureAD.sampleAzureADAuthorizationCodeApp
  | idp == DemoIdp IFacebook.defaultFacebookIdp = Just IFacebook.sampleFacebookAuthorizationCodeApp
  | idp == DemoIdp IFitbit.defaultFitbitIdp = Just IFitbit.sampleFitbitAuthorizationCodeApp
  | idp == DemoIdp IGithub.defaultGithubIdp = Just IGithub.sampleGithubAuthorizationCodeApp
  | idp == DemoIdp IDropbox.defaultDropboxIdp = Just IDropbox.sampleDropboxAuthorizationCodeApp
  | idp == DemoIdp IGoogle.defaultGoogleIdp = Just IGoogle.sampleGoogleAuthorizationCodeApp
  | idp == DemoIdp ILinkedin.defaultLinkedinIdp = Just ILinkedin.sampleLinkedinAuthorizationCodeApp
  | idp == DemoIdp ITwitter.defaultTwitterIdp = Just ITwitter.sampleTwitterAuthorizationCodeApp
  | idp == DemoIdp ISlack.defaultSlackIdp = Just ISlack.sampleSlackAuthorizationCodeApp
  | idp == DemoIdp IWeibo.defaultWeiboIdp = Just IWeibo.sampleWeiboAuthorizationCodeApp
  | idp == DemoIdp IZOHO.defaultZohoIdp = Just IZOHO.sampleZohoAuthorizationCodeApp
  | idp == DemoIdp IStackExchange.defaultStackExchangeIdp = Just IStackExchange.sampleStackExchangeAuthorizationCodeApp
  | otherwise = Nothing

-- | https://auth0.com/docs/api/authentication#resource-owner-password
createResourceOwnerPasswordApp ::
  Idp i ->
  IdpName ->
  ExceptT Text IO (IdpApplication i ResourceOwnerPasswordApplication)
createResourceOwnerPasswordApp i (IdpName idpName) = do
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
  Env.OAuthAppSetting {..} <- Env.lookup newAppName
  newApp' <- case user of
    Nothing -> throwE ("[createResourceOwnerPasswordApp] unable to load user config for " <> idpName)
    Just userConfig ->
      pure
        defaultApp
          { ropClientId = clientId
          , ropClientSecret = clientSecret
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
  IdpName ->
  ExceptT Text IO (IdpApplication i ClientCredentialsApplication)
createClientCredentialsApp i (IdpName idpName) = do
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

  Env.OAuthAppSetting {..} <- Env.lookup newAppName
  newApp <- case idpName of
    "Auth0" ->
      pure
        defaultApp
          { ccTokenRequestExtraParams = Map.fromList [("audience ", "https://freizl.auth0.com/api/v2/")]
          }
    -- "okta" -> createOktaClientCredentialsGrantAppJwt i resp
    _ -> pure defaultApp
  let newApp' =
        newApp
          { ccClientId = clientId
          , ccClientSecret = clientSecret
          , ccScope = scopes
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
  IdpName ->
  ExceptT Text IO (IdpApplication i DeviceAuthorizationApplication)
createDeviceAuthApp i (IdpName idpName) = do
  let authMethod =
        if "Okta" `TL.isInfixOf` idpName
          then Just ClientSecretBasic
          else Nothing
      extraParams =
        if "AzureAD" `TL.isInfixOf` idpName
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
  Env.OAuthAppSetting {..} <- Env.lookup newAppName
  let newApp' =
        newApp
          { daClientId = clientId
          , daClientSecret = clientSecret
          , daScope = scopes
          }
  pure $
    IdpApplication
      { idp = i
      , application = newApp'
      }

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

initSupportedIdps ::
  TenantBasedIdps ->
  Map.Map IdpName DemoIdp
initSupportedIdps (myAuth0Idp, myOktaIdp) =
  Map.fromList
    [ ("AzureAD", DemoIdp IAzureAD.defaultAzureADIdp)
    , ("Auth0", DemoIdp myAuth0Idp)
    , ("Okta", DemoIdp myOktaIdp)
    , ("Facebook", DemoIdp IFacebook.defaultFacebookIdp)
    , ("Fitbit", DemoIdp IFitbit.defaultFitbitIdp)
    , ("GitHub", DemoIdp IGithub.defaultGithubIdp)
    , ("DropBox", DemoIdp IDropbox.defaultDropboxIdp)
    , ("Google", DemoIdp IGoogle.defaultGoogleIdp)
    , ("LinkedIn", DemoIdp ILinkedin.defaultLinkedinIdp)
    , ("Twitter", DemoIdp ITwitter.defaultTwitterIdp)
    , ("Slack", DemoIdp ISlack.defaultSlackIdp)
    , ("Weibo", DemoIdp IWeibo.defaultWeiboIdp)
    , ("Zoho", DemoIdp IZOHO.defaultZohoIdp)
    , ("StackExchange", DemoIdp IStackExchange.defaultStackExchangeIdp)
    ]

isSupportPkce :: IdpName -> Bool
isSupportPkce idpName =
  idpName
    `elem` [ "Auth0"
           , "Okta"
           , "Google"
           , "Twitter"
           ]
