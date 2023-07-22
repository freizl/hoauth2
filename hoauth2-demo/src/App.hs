{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (app) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Bifunctor
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Env
import Idp
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Network.OAuth2.Provider.Auth0 qualified as Auth0
import Network.OAuth2.Provider.Okta qualified as Okta
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Session
import Types
import User
import Utils
import Views
import Web.Scotty
import Web.Scotty qualified as Scotty
import Prelude

-------------------------------------------------------------------------------
--                                    App                                    --
-------------------------------------------------------------------------------

myServerPort :: Int
myServerPort = 9988

app :: IO ()
app =
  putStrLn ("Starting Server. http://localhost:" ++ show myServerPort)
    >> waiApp
    >>= run myServerPort

waiApp :: IO WAI.Application
waiApp = do
  re <- runExceptT $ do
    myAuth0Idp <- Auth0.mkAuth0Idp "freizl.auth0.com"
    myOktaIdp <- Okta.mkOktaIdp "dev-494096.okta.com"
    -- myOktaIdp <- Okta.mkOktaIdp "dev-494096.okta.com/oauth2/default"
    let oidcIdps = (myAuth0Idp, myOktaIdp)
    oauthAppSettings <- readEnvFile
    sessionStore <- liftIO (initUserStore allIdpNames)
    let findIdpByName = findIdp oidcIdps
    pure AppEnv {..}
  case re of
    Left e -> Prelude.error $ TL.unpack $ "unable to init demo server: \n" <> e
    Right r -> initApp r

initApp ::
  AppEnv ->
  IO WAI.Application
initApp appEnv = do
  scottyApp $ do
    middleware $ staticPolicy (addBase "public/assets")
    defaultHandler globalErrorHandler

    get "/" $ indexH appEnv

    -- Authorization Code Grant
    get "/login" $ loginH appEnv
    get "/logout" $ logoutH appEnv
    get "/oauth2/callback" $ callbackH appEnv
    get "/refresh-token" $ refreshTokenH appEnv

    -- Resource Owner Password Grant
    get "/login/password-grant" $ testPasswordGrantTypeH appEnv

    -- Client Credentials Grant
    get "/login/cc-grant" $ testClientCredentialGrantTypeH appEnv

    -- Device Authorization Grant
    get "/login/device-auth-grant" $ testDeviceCodeGrantTypeH appEnv

    -- JWT Grant
    get "/login/jwt-grant" testJwtBearerGrantTypeH

-------------------------------------------------------------------------------
--                                  Handlers                                 --
-------------------------------------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status500 >> html t

indexH ::
  AppEnv ->
  ActionM ()
indexH AppEnv {..} = do
  liftIO (allAppSessionData sessionStore) >>= overviewTpl

loginH ::
  AppEnv ->
  ActionM ()
loginH AppEnv {..} = do
  authRequestUri <- runActionWithIdp "loginH" $ \idpName -> do
    -- TODO: I dont understand why can use let here
    -- let (DemoIdp idp) = findIdpByName idpName
    (DemoIdp idp) <- pure (findIdpByName idpName)
    authCodeApp <- createAuthorizationCodeApp idp idpName
    (authorizationUri, codeVerifier) <-
      liftIO $
        if isSupportPkce idpName
          then fmap (second Just) (mkPkceAuthorizeRequest authCodeApp)
          else pure (mkAuthorizationRequest authCodeApp, Nothing)
    insertCodeVerifier sessionStore idpName codeVerifier
    pure authorizationUri
  Scotty.setHeader "Location" (TL.fromStrict $ uriToText authRequestUri)
  Scotty.status status302

logoutH ::
  AppEnv ->
  ActionM ()
logoutH AppEnv {..} = do
  runActionWithIdp "logoutH" $ \idpName -> do
    liftIO (removeAppSessionData sessionStore idpName)
  redirectToHomeM

callbackH ::
  AppEnv ->
  ActionM ()
callbackH appEnv@AppEnv {..} = do
  -- https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html#t:Param
  -- (Text, Text)
  pas <- params
  let stateP = paramValue "state" pas
  when (null stateP) (raise "callbackH: no state from callback request")
  let codeP = paramValue "code" pas
  when (null codeP) (raise "callbackH: no code from callback request")
  let idpName = fromText $ TL.takeWhile (/= '.') (head stateP)
  exceptToActionM $ do
    idpData <- lookupAppSessionData sessionStore idpName
    fetchTokenAndUser appEnv idpData (ExchangeToken $ TL.toStrict $ head codeP)
  redirectToHomeM

refreshTokenH ::
  AppEnv ->
  ActionM ()
refreshTokenH AppEnv {..} = do
  runActionWithIdp "testPasswordGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- pure (findIdpByName idpName)
    authCodeApp <- createAuthorizationCodeApp idp idpName
    idpData <- lookupAppSessionData sessionStore idpName
    newToken <- doRefreshToken authCodeApp idpData
    liftIO $ do
      putStrLn "=== refreshTokenH === got new token"
      print newToken
      upsertAppSessionData sessionStore idpName (idpData {oauth2Token = Just newToken})
  redirectToHomeM

testPasswordGrantTypeH ::
  AppEnv ->
  ActionM ()
testPasswordGrantTypeH AppEnv {..} = do
  runActionWithIdp "testPasswordGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- pure (findIdpByName idpName)
    idpApp <- createResourceOwnerPasswordApp idp idpName
    mgr <- liftIO $ newManager tlsManagerSettings
    token <- withExceptT tokenRequestErrorErrorToText $ conduitTokenRequest idpApp mgr NoNeedExchangeToken
    user <- tryFetchUser idpName idpApp mgr token
    liftIO $ do
      putStrLn "=== testPasswordGrantTypeH find token ==="
      print user
  redirectToHomeM

testClientCredentialGrantTypeH ::
  AppEnv ->
  ActionM ()
testClientCredentialGrantTypeH AppEnv {..} = do
  runActionWithIdp "testClientCredentialsGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- pure (findIdpByName idpName)
    idpApp <- createClientCredentialsApp idp idpName
    mgr <- liftIO $ newManager tlsManagerSettings
    -- client credentials flow is meant for machine to machine
    -- hence wont be able to hit /userinfo endpoint
    tokenResp <- withExceptT tokenRequestErrorErrorToText $ conduitTokenRequest idpApp mgr NoNeedExchangeToken
    liftIO $ do
      putStrLn "=== testClientCredentialGrantTypeH find token ==="
      print tokenResp
  redirectToHomeM

testDeviceCodeGrantTypeH ::
  AppEnv ->
  ActionM ()
testDeviceCodeGrantTypeH AppEnv {..} = do
  runActionWithIdp "testDeviceCodeGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- pure (findIdpByName idpName)
    deviceAuthApp <- createDeviceAuthApp idp idpName
    mgr <- liftIO $ newManager tlsManagerSettings
    deviceAuthResp <- withExceptT bslToText $ conduitDeviceAuthorizationRequest deviceAuthApp mgr
    liftIO $ do
      putStr "Please visit this URL to redeem the code: "
      TL.putStr $ userCode deviceAuthResp <> "\n"
      TL.putStrLn $ TL.fromStrict $ uriToText (verificationUri deviceAuthResp)
    atoken <- withExceptT tokenRequestErrorErrorToText (pollDeviceTokenRequest deviceAuthApp mgr deviceAuthResp)
    liftIO $ do
      putStrLn "=== testDeviceCodeGrantTypeH found token ==="
      print atoken
    luser <- tryFetchUser idpName deviceAuthApp mgr atoken
    liftIO $ print luser
  redirectToHomeM

-- Only testing google for now
testJwtBearerGrantTypeH :: ActionM ()
testJwtBearerGrantTypeH = do
  exceptToActionM $ do
    testApp <- googleServiceAccountApp
    mgr <- liftIO $ newManager tlsManagerSettings
    tokenResp <- withExceptT tokenRequestErrorErrorToText $ conduitTokenRequest testApp mgr NoNeedExchangeToken
    user <- tryFetchUser Google testApp mgr tokenResp
    liftIO $ print user
  redirectToHomeM

-------------------------------------------------------------------------------
--                                  Services                                 --
-------------------------------------------------------------------------------

exceptToActionM :: ExceptT Text IO a -> ActionM a
exceptToActionM e = do
  result <- liftIO $ runExceptT e
  either raise return result

runActionWithIdp ::
  Text ->
  (IdpName -> ExceptT Text IO a) ->
  ActionM a
runActionWithIdp funcName action = do
  midp <- paramValueMaybe "idp" <$> params
  case midp of
    Just idpName -> exceptToActionM (action $ fromText idpName)
    Nothing -> raise $ "[" <> funcName <> "] Expects 'idp' parameter but found nothing"

fetchTokenAndUser ::
  AppEnv ->
  IdpAuthorizationCodeAppSessionData ->
  -- | Session Data
  ExchangeToken ->
  ExceptT Text IO ()
fetchTokenAndUser AppEnv {..} idpData@(IdpAuthorizationCodeAppSessionData {..}) exchangeToken = do
  (DemoIdp idp) <- pure (findIdpByName idpName)
  authCodeIdpApp <- createAuthorizationCodeApp idp idpName
  mgr <- liftIO $ newManager tlsManagerSettings
  token <- tryFetchAccessToken authCodeIdpApp mgr exchangeToken
  liftIO $ do
    putStrLn "[Authorization Code Flow] Found access token"
    print token
  luser <- tryFetchUser idpName authCodeIdpApp mgr token
  liftIO $ do
    print luser
    upsertAppSessionData
      sessionStore
      idpName
      (idpData {loginUser = Just luser, oauth2Token = Just token})
  where
    tryFetchAccessToken ::
      IdpApplication i AuthorizationCodeApplication ->
      Manager ->
      ExchangeToken ->
      ExceptT Text IO OAuth2Token
    tryFetchAccessToken idpApp mgr exchangeTokenText = do
      if isSupportPkce idpName
        then do
          when (isNothing authorizePkceCodeVerifier) (throwE "Unable to find code verifier")
          withExceptT tokenRequestErrorErrorToText $
            conduitPkceTokenRequest
              idpApp
              mgr
              (exchangeTokenText, fromJust authorizePkceCodeVerifier)
        else withExceptT tokenRequestErrorErrorToText $ conduitTokenRequest idpApp mgr exchangeTokenText

tokenRequestErrorErrorToText :: TokenRequestError -> Text
tokenRequestErrorErrorToText e = TL.pack $ "conduitTokenRequest - cannot fetch access token. error detail: " ++ show e

tryFetchUser ::
  forall i a.
  (HasUserInfoRequest a, HasDemoLoginUser i, FromJSON (IdpUserInfo i)) =>
  IdpName ->
  IdpApplication i a ->
  Manager ->
  OAuth2Token ->
  ExceptT Text IO DemoLoginUser
tryFetchUser idpName idpAppConfig mgr at = do
  let fetchMethod = findFetchUserInfoMethod idpName
  user <- withExceptT bslToText $ do
    fetchMethod idpAppConfig mgr (accessToken at)
  pure $ toLoginUser @i user

doRefreshToken ::
  IdpApplication i AuthorizationCodeApplication ->
  IdpAuthorizationCodeAppSessionData ->
  ExceptT Text IO OAuth2Token
doRefreshToken idpAppConfig (IdpAuthorizationCodeAppSessionData {..}) = do
  at <- maybe (throwE "no token response found for idp") pure oauth2Token
  rt <- maybe (throwE "no refresh token found for idp") pure (OAuth2.refreshToken at)
  withExceptT (TL.pack . show) $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    conduitRefreshTokenRequest idpAppConfig mgr rt
