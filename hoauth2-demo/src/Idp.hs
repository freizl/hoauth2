{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Idp where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Contrib
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Env qualified
import Jose.Jwt
import Network.HTTP.Conduit
import Network.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Network.OAuth2.Provider.Auth0 qualified as IAuth0
import Network.OAuth2.Provider.AzureAD qualified as IAzureAD
import Network.OAuth2.Provider.DropBox qualified as IDropBox
import Network.OAuth2.Provider.Facebook qualified as IFacebook
import Network.OAuth2.Provider.Fitbit qualified as IFitbit
import Network.OAuth2.Provider.GitHub qualified as IGitHub
import Network.OAuth2.Provider.Google qualified as IGoogle
import Network.OAuth2.Provider.Linear qualified as ILinear
import Network.OAuth2.Provider.LinkedIn qualified as ILinkedIn
import Network.OAuth2.Provider.Okta qualified as IOkta
import Network.OAuth2.Provider.Slack qualified as ISlack
import Network.OAuth2.Provider.StackExchange qualified as IStackExchange
import Network.OAuth2.Provider.Twitter qualified as ITwitter
import Network.OAuth2.Provider.Weibo qualified as IWeibo
import Network.OAuth2.Provider.ZOHO qualified as IZOHO
import Types
import URI.ByteString (URI, URIRef (uriPath))
import URI.ByteString.QQ (uri)
import User
import Prelude hiding (id)

defaultOAuth2RedirectUri :: URI
defaultOAuth2RedirectUri = [uri|http://localhost:9988/oauth2/callback|]

createAuthorizationCodeApp ::
  Idp i ->
  IdpName ->
  ExceptT Text IO (IdpApplication i AuthorizationCodeApplication)
createAuthorizationCodeApp idp idpName = do
  let newAppName = "sample-" <> toText idpName <> "-authorization-code-app"
  let sampleApp = findAuthorizationCodeSampleApp idpName
  Env.OAuthApp {..} <- Env.lookupApp newAppName
  let newApp' =
        sampleApp
          { acClientId = clientId
          , acClientSecret = clientSecret
          , acScope = if Set.null scopes then acScope sampleApp else scopes
          , acRedirectUri = defaultOAuth2RedirectUri
          , acAuthorizeState = AuthorizeState (toText idpName <> ".hoauth2-demo-app-123")
          }
  pure IdpApplication {idp = idp, application = newApp'}

-- | https://auth0.com/docs/api/authentication#resource-owner-password
createResourceOwnerPasswordApp ::
  Idp i ->
  IdpName ->
  ExceptT Text IO (IdpApplication i ResourceOwnerPasswordApplication)
createResourceOwnerPasswordApp i idpName = do
  let newAppName = "sample-" <> toText idpName <> "-resource-owner-app"
  let defaultApp =
        ResourceOwnerPasswordApplication
          { ropClientId = ""
          , ropClientSecret = ""
          , ropName = newAppName
          , ropScope = Set.fromList ["openid", "profile", "email"]
          , ropUserName = ""
          , ropPassword = ""
          , ropTokenRequestExtraParams = Map.empty
          , ropClientAuthenticationMethod = ClientSecretBasic
          }
  Env.OAuthApp {..} <- Env.lookupApp newAppName
  newApp' <- case user of
    Nothing -> throwE ("[createResourceOwnerPasswordApp] unable to load user config for " <> toText idpName)
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
createClientCredentialsApp i idpName = do
  let newAppName = "sample-" <> toText idpName <> "-client-credentials-app"
  let defaultApp =
        ClientCredentialsApplication
          { ccClientId = ""
          , ccClientSecret = ""
          , ccClientAuthenticationMethod = ClientSecretBasic
          , ccName = ""
          , ccScope = Set.empty
          , ccTokenRequestExtraParams = Map.empty
          }

  appSetting@Env.OAuthApp {..} <- Env.lookupApp newAppName
  newApp <- case idpName of
    Auth0 -> do
      auth0Domain <- Env.lookupAuth0Domain
      let audience = "https://" <> auth0Domain <> "/api/v2/"
      pure
        defaultApp
          { ccTokenRequestExtraParams = Map.fromList [("audience", audience)]
          , ccClientId = clientId
          , ccClientSecret = clientSecret
          , ccScope = scopes
          , ccName = newAppName
          }
    Okta -> do
      -- ORG AS only support private key jwt
      -- Custom AS support both. In this demo app, just default to client secret
      if uriPath (idpTokenEndpoint i) == "/oauth2/v1/token"
        then createOktaClientCredentialsGrantAppJwt i appSetting
        else
          pure
            defaultApp
              { ccClientId = clientId
              , ccClientSecret = clientSecret
              , ccScope = scopes
              , ccName = newAppName
              }
    _ -> pure defaultApp
  pure $
    IdpApplication
      { idp = i
      , application = newApp
      }

-- Base on the document, it works well with both custom Athourization Server and Org As.
-- https://developer.okta.com/docs/guides/implement-grant-type/clientcreds/main/#client-credentials-flow
--
-- But with Org AS, has to use jwt athentication method otherwise got error
-- Client Credentials requests to the Org Authorization Server must use the private_key_jwt token_endpoint_auth_method
--
-- FIXME: Get error from Okta "The parsing of the client_assertion failed."
--
createOktaClientCredentialsGrantAppJwt ::
  Idp i ->
  Env.OAuthApp ->
  ExceptT Text IO ClientCredentialsApplication
createOktaClientCredentialsGrantAppJwt i Env.OAuthApp {..} = do
  keyJsonStr <- liftIO $ BS.readFile ".okta-key.json"
  jwk <- except (first TL.pack $ Aeson.eitherDecodeStrict keyJsonStr)
  jwt <- ExceptT $ IOkta.mkOktaClientCredentialAppJwt jwk clientId i
  pure
    ClientCredentialsApplication
      { ccClientId = clientId
      , ccClientSecret = ClientSecret (TL.decodeUtf8 $ bsFromStrict $ unJwt jwt)
      , ccClientAuthenticationMethod = ClientAssertionJwt
      , ccName = ""
      , ccScope = scopes
      , ccTokenRequestExtraParams = Map.empty
      }

createDeviceAuthApp ::
  Idp i ->
  IdpName ->
  ExceptT Text IO (IdpApplication i DeviceAuthorizationApplication)
createDeviceAuthApp i idpName = do
  let authMethod =
        if Okta == idpName
          then ClientSecretBasic
          else ClientSecretPost
      extraParams =
        if AzureAD == idpName
          then Map.singleton "tenant" "/common"
          else Map.empty
  let newAppName = "sample-" <> toText idpName <> "-device-authorization-app"
      newApp =
        DeviceAuthorizationApplication
          { daClientId = ""
          , daClientSecret = ""
          , daName = newAppName
          , daScope = Set.empty
          , daAuthorizationRequestExtraParam = extraParams
          , daAuthorizationRequestAuthenticationMethod = authMethod
          }
  Env.OAuthApp {..} <- Env.lookupApp newAppName
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

googleServiceAccountApp :: ExceptT Text IO (IdpApplication Google JwtBearerApplication)
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

findIdp ::
  TenantBasedIdps ->
  IdpName ->
  DemoIdp
findIdp (myAuth0Idp, myOktaIdp) = \case
  AzureAD -> DemoIdp IAzureAD.defaultAzureADIdp
  Auth0 -> DemoIdp myAuth0Idp
  Okta -> DemoIdp myOktaIdp
  Facebook -> DemoIdp IFacebook.defaultFacebookIdp
  Fitbit -> DemoIdp IFitbit.defaultFitbitIdp
  GitHub -> DemoIdp IGitHub.defaultGithubIdp
  DropBox -> DemoIdp IDropBox.defaultDropBoxIdp
  Google -> DemoIdp IGoogle.defaultGoogleIdp
  Linear -> DemoIdp ILinear.defaultLinearIdp
  LinkedIn -> DemoIdp ILinkedIn.defaultLinkedInIdp
  Twitter -> DemoIdp ITwitter.defaultTwitterIdp
  Slack -> DemoIdp ISlack.defaultSlackIdp
  Weibo -> DemoIdp IWeibo.defaultWeiboIdp
  ZOHO -> DemoIdp IZOHO.defaultZohoIdp
  StackExchange -> DemoIdp (IStackExchange.defaultStackExchangeIdp "xyz")

findAuthorizationCodeSampleApp :: IdpName -> AuthorizationCodeApplication
findAuthorizationCodeSampleApp = \case
  Auth0 -> IAuth0.sampleAuth0AuthorizationCodeApp
  Okta -> IOkta.sampleOktaAuthorizationCodeApp
  AzureAD -> IAzureAD.sampleAzureADAuthorizationCodeApp
  Facebook -> IFacebook.sampleFacebookAuthorizationCodeApp
  Fitbit -> IFitbit.sampleFitbitAuthorizationCodeApp
  GitHub -> IGitHub.sampleGithubAuthorizationCodeApp
  DropBox -> IDropBox.sampleDropBoxAuthorizationCodeApp
  Google -> IGoogle.sampleGoogleAuthorizationCodeApp
  Linear -> ILinear.sampleLinearAuthorizationCodeApp
  LinkedIn -> ILinkedIn.sampleLinkedInAuthorizationCodeApp
  Twitter -> ITwitter.sampleTwitterAuthorizationCodeApp
  Slack -> ISlack.sampleSlackAuthorizationCodeApp
  Weibo -> IWeibo.sampleWeiboAuthorizationCodeApp
  ZOHO -> IZOHO.sampleZohoAuthorizationCodeApp
  StackExchange -> IStackExchange.sampleStackExchangeAuthorizationCodeApp

findFetchUserInfoMethod ::
  (MonadIO m, HasDemoLoginUser i, HasUserInfoRequest a, FromJSON (IdpUser i)) =>
  IdpName ->
  ( IdpApplication i a ->
    Manager ->
    AccessToken ->
    ExceptT BSL.ByteString m (IdpUser i)
  )
findFetchUserInfoMethod = \case
  Auth0 -> IAuth0.fetchUserInfo
  Okta -> IOkta.fetchUserInfo
  AzureAD -> IAzureAD.fetchUserInfo
  Facebook -> IFacebook.fetchUserInfo
  Fitbit -> IFitbit.fetchUserInfo
  GitHub -> IGitHub.fetchUserInfo
  DropBox -> IDropBox.fetchUserInfo
  Google -> IGoogle.fetchUserInfo
  Linear -> ILinear.fetchUserInfo
  LinkedIn -> ILinkedIn.fetchUserInfo
  Twitter -> ITwitter.fetchUserInfo
  Slack -> ISlack.fetchUserInfo
  Weibo -> IWeibo.fetchUserInfo
  ZOHO -> IZOHO.fetchUserInfo
  StackExchange -> IStackExchange.fetchUserInfo

isSupportPkce :: IdpName -> Bool
isSupportPkce idpName =
  idpName
    `elem` [ Auth0
           , Okta
           , Google
           , Twitter
           ]
