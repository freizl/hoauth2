{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module App
  ( app,
    waiApp,
  )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import IDP
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest qualified as TR
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

-- TODO: how to add either Monad or a middleware to do server side session?
waiApp :: IO WAI.Application
waiApp = do
  cache <- initCacheStore
  initIdps cache
  scottyApp $ do
    middleware $ staticPolicy (addBase "public/assets")
    defaultHandler globalErrorHandler
    get "/" $ indexH cache
    get "/oauth2/callback" $ callbackH cache
    get "/logout" $ logoutH cache
    get "/refresh" $ refreshH cache

--------------------------------------------------

-- * Handlers

--------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status500 >> html t

refreshH :: CacheStore -> ActionM ()
refreshH c = do
  idpData <- readIdpParam c
  exceptToActionM $ do
    newToken <- doRefreshToken idpData
    liftIO $ do
      putStrLn "got new token"
      upsertIDPData c (idpData {oauth2Token = Just newToken})
  -- TODO: double check: shall only return to home when no error.
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

-- NOTE: looks like `state` shall be passed when fetching access token
--       turns out no IDP enforce this yet

--------------------------------------------------

-- * Services

--------------------------------------------------

exceptToActionM :: Show a => ExceptT Text IO a -> ActionM a
exceptToActionM e = do
  result <- liftIO $ runExceptT e
  either raise return result

readIdpParam :: CacheStore -> ActionM IDPData
readIdpParam c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  exceptToActionM $ lookupKey c (head idpP)

fetchTokenAndUser ::
  CacheStore ->
  Text ->
  IDPData ->
  ExceptT Text IO ()
fetchTokenAndUser c code idpData@(IDPData (IDPApp idp) _ _) = do
  mgr <- liftIO $ newManager tlsManagerSettings
  token <-
    withExceptT oauth2ErrorToText $
      tokenReq idp mgr (ExchangeToken $ TL.toStrict code)
  -- liftIO $ print token
  (luser, at) <- tryFetchUser mgr token idp
  liftIO $ updateIdp c idpData luser at
  where
    oauth2ErrorToText :: OAuth2Error TR.Errors -> Text
    oauth2ErrorToText e = TL.pack $ "tokenReq - cannot fetch asses token. error detail: " ++ show e
    updateIdp :: CacheStore -> IDPData -> LoginUser -> OAuth2Token -> IO ()
    updateIdp c1 oldIdpData luser token =
      upsertIDPData
        c1
        (oldIdpData {loginUser = Just luser, oauth2Token = Just token})

tryFetchUser ::
  IsIDP a =>
  Manager ->
  OAuth2Token ->
  a ->
  ExceptT Text IO (LoginUser, OAuth2Token)
tryFetchUser mgr at idp = do
  user <- withExceptT bslToText $ userReq idp mgr (accessToken at)
  return (user, at)

doRefreshToken :: IDPData -> ExceptT Text IO OAuth2Token
doRefreshToken (IDPData (IDPApp idp) _ token) = do
  case token of
    Nothing -> throwE "no token found for idp"
    Just at -> case refreshToken at of
      Nothing -> throwE "no refresh token presents. did you add 'offline_access' scope?"
      Just rt -> withExceptT (TL.pack . show) $ do
        mgr <- liftIO $ newManager tlsManagerSettings
        tokenRefreshReq idp mgr rt
