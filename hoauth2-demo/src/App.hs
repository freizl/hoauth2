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
import Idp
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider.Auth0 qualified as IAuth0
import Network.OAuth2.Provider.Okta qualified as IOkta
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static
import Session
import Types
import Utils
import Views
import Web.Scotty
import Web.Scotty qualified as Scotty
import Prelude

------------------------------
-- App
------------------------------

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
    myAuth0Idp <- IAuth0.mkAuth0Idp "freizl.auth0.com"
    myOktaIdp <- IOkta.mkOktaIdp "dev-494096.okta.com"
    -- myOktaIdp <- IOkta.mkOktaIdp "dev-494096.okta.com/oauth2/default"
    pure (myAuth0Idp, myOktaIdp)
  case re of
    Left e -> Prelude.error $ TL.unpack $ "unable to init idp via oidc well-known endpoint " <> e
    Right r -> initUserStore >>= initApp r

initApp ::
  (Idp IAuth0.Auth0, Idp IOkta.Okta) ->
  AuthorizationGrantUserStore ->
  IO WAI.Application
initApp idps userStore = do
  scottyApp $ do
    middleware $ staticPolicy (addBase "public/assets")
    defaultHandler globalErrorHandler

    get "/" $ indexH userStore
    get "/login" $ loginH userStore idps
    get "/oauth2/callback" $ callbackH userStore idps
    get "/logout" $ logoutH userStore
    get "/refresh-token" $ refreshTokenH userStore idps

    get "/login/password-grant" $ testPasswordGrantTypeH idps
    get "/login/cc-grant" (testClientCredentialGrantTypeH idps)

    get "/login/jwt-grant" testJwtBearerGrantTypeH
    get "/login/device-auth-grant" $ testDeviceCodeGrantTypeH idps

--------------------------------------------------

-- * Handlers

--------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status500 >> html t

indexH ::
  AuthorizationGrantUserStore ->
  ActionM ()
indexH store = do
  liftIO (allValues store) >>= overviewTpl

loginH :: AuthorizationGrantUserStore -> TenantBasedIdps -> ActionM ()
loginH s idps = do
  authRequestUri <- runActionWithIdp "loginH" $ \idpName -> do
    (DemoIdp idp) <- findIdp idps idpName
    authCodeApp <- createAuthorizationCodeApp idp idpName
    (authorizationUri, codeVerifier) <-
      liftIO $
        if isSupportPkce idpName
          then fmap (second Just) (mkPkceAuthorizeRequest authCodeApp)
          else pure (mkAuthorizationRequest authCodeApp, Nothing)
    insertCodeVerifier s idpName codeVerifier
    pure authorizationUri
  liftIO (print $ uriToText authRequestUri)
  Scotty.setHeader "Location" (TL.fromStrict $ uriToText authRequestUri)
  Scotty.status status302

logoutH :: AuthorizationGrantUserStore -> ActionM ()
logoutH s = do
  runActionWithIdp "logoutH" $ \idpName -> do
    liftIO (removeKey s idpName)
  redirectToHomeM

callbackH ::
  AuthorizationGrantUserStore ->
  TenantBasedIdps ->
  ActionM ()
callbackH s idps = do
  -- https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html#t:Param
  -- (Text, Text)
  pas <- params
  let stateP = paramValue "state" pas
  when (null stateP) (raise "callbackH: no state from callback request")
  let codeP = paramValue "code" pas
  when (null codeP) (raise "callbackH: no code from callback request")
  let idpName = TL.takeWhile (/= '.') (head stateP)
  exceptToActionM $ do
    idpData <- lookupKey s idpName
    fetchTokenAndUser s idps idpData (ExchangeToken $ TL.toStrict $ head codeP)
  redirectToHomeM

refreshTokenH ::
  AuthorizationGrantUserStore ->
  TenantBasedIdps ->
  ActionM ()
refreshTokenH c idps = do
  runActionWithIdp "testPasswordGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- findIdp idps idpName
    authCodeApp <- createAuthorizationCodeApp idp idpName
    idpData <- lookupKey c idpName
    newToken <- doRefreshToken authCodeApp idpData
    liftIO $ do
      putStrLn "=== refreshTokenH === got new token"
      print newToken
      upsertDemoUserData c idpName (idpData {oauth2Token = Just newToken})
  redirectToHomeM

testPasswordGrantTypeH :: (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ActionM ()
testPasswordGrantTypeH idps = do
  runActionWithIdp "testPasswordGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- findIdp idps idpName
    idpApp <- createResourceOwnerPasswordApp idp idpName
    mgr <- liftIO $ newManager tlsManagerSettings
    token <- withExceptT oauth2ErrorToText $ conduitTokenRequest idpApp mgr NoNeedExchangeToken
    user <- tryFetchUser mgr token idpApp
    liftIO $ do
      putStrLn "=== testPasswordGrantTypeH find token ==="
      print user
  redirectToHomeM

testClientCredentialGrantTypeH ::
  (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ActionM ()
testClientCredentialGrantTypeH idps = do
  runActionWithIdp "testClientCredentialsGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- findIdp idps idpName
    idpApp <- createClientCredentialsApp idp idpName
    mgr <- liftIO $ newManager tlsManagerSettings
    -- client credentials flow is meant for machine to machine
    -- hence wont be able to hit /userinfo endpoint
    tokenResp <- withExceptT oauth2ErrorToText $ conduitTokenRequest idpApp mgr NoNeedExchangeToken
    liftIO $ do
      putStrLn "=== testClientCredentialGrantTypeH find token ==="
      print tokenResp
  redirectToHomeM

testDeviceCodeGrantTypeH ::
  (Idp IAuth0.Auth0, Idp IOkta.Okta) ->
  ActionM ()
testDeviceCodeGrantTypeH idps = do
  runActionWithIdp "testDeviceCodeGrantTypeH" $ \idpName -> do
    (DemoIdp idp) <- findIdp idps idpName
    testApp <- createDeviceAuthApp idp idpName
    mgr <- liftIO $ newManager tlsManagerSettings
    deviceAuthResp <- withExceptT bslToText $ conduitDeviceAuthorizationRequest testApp mgr
    liftIO $ do
      putStr "Please visit this URL to redeem the code: "
      TL.putStr $ userCode deviceAuthResp <> "\n"
      TL.putStrLn $ TL.fromStrict $ uriToText (verificationUri deviceAuthResp)
    atoken <- withExceptT oauth2ErrorToText (pollDeviceTokenRequest testApp mgr deviceAuthResp)
    liftIO $ do
      putStrLn "=== testDeviceCodeGrantTypeH found token ==="
      print atoken
    luser <- tryFetchUser mgr atoken testApp
    liftIO $ print luser
  redirectToHomeM

-- Only testing google for now
testJwtBearerGrantTypeH :: ActionM ()
testJwtBearerGrantTypeH = do
  exceptToActionM $ do
    testApp <- googleServiceAccountApp
    mgr <- liftIO $ newManager tlsManagerSettings
    tokenResp <- withExceptT oauth2ErrorToText $ conduitTokenRequest testApp mgr NoNeedExchangeToken
    user <- tryFetchUser mgr tokenResp testApp
    liftIO $ print user
  redirectToHomeM

--------------------------------------------------

-- * Services

--------------------------------------------------

exceptToActionM :: ExceptT Text IO a -> ActionM a
exceptToActionM e = do
  result <- liftIO $ runExceptT e
  either raise return result

-- (Idp IAuth0.Auth0, Idp IOkta.Okta)
runActionWithIdp ::
  -- forall a.
  -- Show a =>
  Text ->
  (Text -> ExceptT Text IO a) ->
  ActionM a
runActionWithIdp funcName action = do
  midp <- paramValueMaybe "idp" <$> params
  case midp of
    Just idpName -> exceptToActionM (action idpName)
    Nothing -> raise $ "[" <> funcName <> "] Expects 'idp' parameter but found nothing"

-- readIdpParam :: CacheStore -> ActionM DemoAppEnv
-- readIdpParam c = do
--   pas <- params
--   let idpP = paramValue "idp" pas
--   when (null idpP) redirectToHomeM
--   exceptToActionM $ lookupKey c (head idpP)

fetchTokenAndUser ::
  AuthorizationGrantUserStore ->
  TenantBasedIdps ->
  DemoAppPerAppSessionData ->
  -- | Session Data
  ExchangeToken ->
  ExceptT Text IO ()
fetchTokenAndUser c idps idpData@(DemoAppPerAppSessionData {..}) exchangeToken = do
  (DemoIdp idp) <- findIdp idps idpName
  authCodeIdpApp <- createAuthorizationCodeApp idp idpName
  mgr <- liftIO $ newManager tlsManagerSettings
  token <- tryFetchAccessToken authCodeIdpApp mgr exchangeToken
  liftIO $ do
    putStrLn "[Authorization Code Flow] Found access token"
    print token
  luser <- tryFetchUser mgr token authCodeIdpApp
  liftIO $ do
    print luser
    upsertDemoUserData
      c
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
          withExceptT oauth2ErrorToText $
            conduitPkceTokenRequest
              idpApp
              mgr
              (exchangeTokenText, fromJust authorizePkceCodeVerifier)
        else withExceptT oauth2ErrorToText $ conduitTokenRequest idpApp mgr exchangeTokenText

oauth2ErrorToText :: TokenRequestError -> Text
oauth2ErrorToText e = TL.pack $ "conduitTokenRequest - cannot fetch access token. error detail: " ++ show e

tryFetchUser ::
  forall i a.
  (HasDemoLoginUser i, HasUserInfoRequest a, FromJSON (IdpUserInfo i)) =>
  Manager ->
  OAuth2Token ->
  IdpApplication i a ->
  ExceptT Text IO DemoLoginUser
tryFetchUser mgr at idpAppConfig = do
  user <- withExceptT bslToText $ conduitUserInfoRequest idpAppConfig mgr (accessToken at)
  pure $ toLoginUser @i user

doRefreshToken ::
  IdpApplication i AuthorizationCodeApplication ->
  DemoAppPerAppSessionData ->
  ExceptT Text IO OAuth2Token
doRefreshToken idpAppConfig (DemoAppPerAppSessionData {..}) = do
  at <- maybe (throwE "no token response found for idp") pure oauth2Token
  rt <- maybe (throwE "no refresh token found for idp") pure (OAuth2.refreshToken at)
  withExceptT (TL.pack . show) $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    conduitRefreshTokenRequest idpAppConfig mgr rt
