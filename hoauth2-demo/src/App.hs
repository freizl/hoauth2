{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module App (app) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Idp
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2 qualified as OAuth2
import Network.OAuth.OAuth2.TokenRequest qualified as TR
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
  cache <- initCacheStore
  re <- runExceptT $ do
    myAuth0Idp <- IAuth0.mkAuth0Idp "freizl.auth0.com"
    myOktaIdp <- IOkta.mkOktaIdp "dev-494096.okta.com"
    -- For the sake of simplicity for this demo App,
    -- I store user data in MVar in server side.
    -- It means user session shared across browsers.
    -- which simplify my testing cross browsers.
    -- I am sure you don't want to this for your production services.
    initIdps cache (myAuth0Idp, myOktaIdp)
    pure (myAuth0Idp, myOktaIdp)
  case re of
    Left e -> Prelude.error $ TL.unpack $ "unable to init cache: " <> e
    Right r -> do
      putStrLn "global cache has been initialized."
      initApp cache r

initApp :: CacheStore -> (Idp IAuth0.Auth0, Idp IOkta.Okta) -> IO WAI.Application
initApp cache idps = scottyApp $ do
  middleware $ staticPolicy (addBase "public/assets")
  defaultHandler globalErrorHandler
  get "/" $ indexH cache
  get "/oauth2/callback" $ callbackH cache
  get "/logout" $ logoutH cache
  get "/refresh" $ refreshH cache

  get "/login/password-grant" $ testPasswordGrantTypeH idps
  get "/login/cc-grant" (testClientCredentialGrantTypeH idps)

--------------------------------------------------

-- * Handlers

--------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status500 >> html t

refreshH :: CacheStore -> ActionM ()
refreshH c = do
  idpData@(DemoAppEnv idp sData) <- readIdpParam c
  exceptToActionM $ do
    newToken <- doRefreshToken idpData
    liftIO $ putStrLn ">>>>>> got new token"
    upsertDemoAppEnv c (DemoAppEnv idp (sData {oauth2Token = Just newToken}))
  redirectToHomeM

logoutH :: CacheStore -> ActionM ()
logoutH c = do
  idpData <- readIdpParam c
  liftIO (removeKey c (toLabel idpData))
  redirectToHomeM

indexH :: CacheStore -> ActionM ()
indexH c = liftIO (allValues c) >>= overviewTpl

callbackH :: CacheStore -> ActionM ()
callbackH c = do
  -- https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html#t:Param
  -- (Text, Text)
  pas <- params
  let stateP = paramValue "state" pas
  when (null stateP) (raise "callbackH: no state from callback request")
  let codeP = paramValue "code" pas
  when (null codeP) (raise "callbackH: no code from callback request")
  exceptToActionM $ do
    idpData <- lookupKey c (TL.takeWhile (/= '.') (head stateP))
    fetchTokenAndUser c (head codeP) idpData
  redirectToHomeM

testPasswordGrantTypeH :: (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ActionM ()
testPasswordGrantTypeH (auth0, okta) = do
  idpP <- paramValue "i" <$> params
  when (null idpP) (raise "testPasswordGrantTypeH: no idp parameter in the password grant type login request")
  let i = head idpP
  case i of
    "auth0" -> testPasswordGrantType (auth0PasswordGrantApp auth0)
    "okta" -> testPasswordGrantType (oktaPasswordGrantApp okta)
    _ -> raise $ "unable to find password grant type flow for idp " <> i
  where
    testPasswordGrantType ::
      ( HasTokenRequest a,
        'ResourceOwnerPassword ~ a,
        HasDemoLoginUser b,
        HasUserInfoRequest a,
        FromJSON (IdpUserInfo b)
      ) =>
      IdpApplication a b ->
      ActionM ()
    testPasswordGrantType idpApp = do
      exceptToActionM $ do
        mgr <- liftIO $ newManager tlsManagerSettings
        token <- withExceptT oauth2ErrorToText $ conduitTokenRequest idpApp mgr
        user <- tryFetchUser mgr token idpApp
        liftIO $ print user
      redirectToHomeM

testClientCredentialGrantTypeH :: (Idp IAuth0.Auth0, Idp IOkta.Okta) -> ActionM ()
testClientCredentialGrantTypeH (auth0, okta) = do
  idpP <- paramValue "i" <$> params
  when (null idpP) (raise "testClientCredentialsGrantTypeH: no idp parameter in the password grant type login request")
  let i = head idpP
  case i of
    "auth0" -> testClientCredentialsGrantType (auth0ClientCredentialsGrantApp auth0)
    "okta" -> testClientCredentialsGrantType (oktaClientCredentialsGrantApp okta)
    _ -> raise $ "unable to find password grant type flow for idp " <> i

testClientCredentialsGrantType ::
  forall a b.
  ( 'ClientCredentials ~ b
  ) =>
  HasTokenRequest b =>
  IdpApplication b a ->
  ActionM ()
testClientCredentialsGrantType testApp = do
  exceptToActionM $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    -- client credentials flow is meant for machine to machine
    -- hence wont be able to hit /userinfo endpoint
    tokenResp <- withExceptT oauth2ErrorToText $ conduitTokenRequest testApp mgr
    liftIO $ print tokenResp
  redirectToHomeM

--------------------------------------------------

-- * Services

--------------------------------------------------

exceptToActionM :: Show a => ExceptT Text IO a -> ActionM a
exceptToActionM e = do
  result <- liftIO $ runExceptT e
  either raise return result

readIdpParam :: CacheStore -> ActionM DemoAppEnv
readIdpParam c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  exceptToActionM $ lookupKey c (head idpP)

fetchTokenAndUser ::
  CacheStore ->
  Text ->
  DemoAppEnv ->
  ExceptT Text IO ()
fetchTokenAndUser c exchangeToken idpData@(DemoAppEnv (DemoAuthorizationApp idpAppConfig) DemoAppPerAppSessionData {..}) = do
  mgr <- liftIO $ newManager tlsManagerSettings
  token <-
    if isSupportPkce idpAppConfig
      then do
        when (isNothing authorizePkceCodeVerifier) (throwE "Unable to find code verifier")
        withExceptT oauth2ErrorToText $
          conduitPkceTokenRequest
            idpAppConfig
            mgr
            (ExchangeToken $ TL.toStrict exchangeToken, fromJust authorizePkceCodeVerifier)
      else withExceptT oauth2ErrorToText $ conduitTokenRequest idpAppConfig mgr (ExchangeToken $ TL.toStrict exchangeToken)

  luser <- tryFetchUser mgr token idpAppConfig
  updateIdp c idpData luser token
  where
    updateIdp :: MonadIO m => CacheStore -> DemoAppEnv -> DemoLoginUser -> OAuth2Token -> ExceptT Text m ()
    updateIdp c1 (DemoAppEnv iApp sData) luser token =
      upsertDemoAppEnv
        c1
        (DemoAppEnv iApp $ sData {loginUser = Just luser, oauth2Token = Just token})

oauth2ErrorToText :: OAuth2Error TR.Errors -> Text
oauth2ErrorToText e = TL.pack $ "mkTokenRequest - cannot fetch access token. error detail: " ++ show e

tryFetchUser ::
  forall a b.
  (HasDemoLoginUser a, HasUserInfoRequest b, FromJSON (IdpUserInfo a)) =>
  Manager ->
  OAuth2Token ->
  IdpApplication b a ->
  ExceptT Text IO DemoLoginUser
tryFetchUser mgr at idpAppConfig = do
  user <- withExceptT bslToText $ conduitUserInfoRequest idpAppConfig mgr (accessToken at)
  pure $ toLoginUser @a user

doRefreshToken :: DemoAppEnv -> ExceptT Text IO OAuth2Token
doRefreshToken (DemoAppEnv (DemoAuthorizationApp idpAppConfig) (DemoAppPerAppSessionData {..})) = do
  at <- maybe (throwE "no token response found for idp") pure oauth2Token
  rt <- maybe (throwE "no refresh token found for idp") pure (OAuth2.refreshToken at)
  withExceptT (TL.pack . show) $ do
    mgr <- liftIO $ newManager tlsManagerSettings
    conduitRefreshTokenRequest idpAppConfig mgr rt
