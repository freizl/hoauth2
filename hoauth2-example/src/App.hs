{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
import qualified Data.Text.Lazy as TL
import IDP
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.OAuth.OAuth2
import qualified Network.Wai as WAI
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

-- TODO: how to add either Monad or a middleware to do session?
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
globalErrorHandler t = status status401 >> html t

refreshH :: CacheStore -> ActionM ()
refreshH c = do
  idpData <- readIdpParam c
  exceptToActionM $ do
    newToken <- doRefreshToken idpData
    liftIO $ do
      putStrLn "got new token"
      print newToken
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
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (raise "callbackH: no code from callback request")
  when (null stateP) (raise "callbackH: no state from callback request")
  exceptToActionM $ do
    idpData <- lookupKey c (TL.takeWhile (/= '.') (head stateP))
    fetchTokenAndUser c (head codeP) idpData
  redirectToHomeM

-- TODO: looks like `state` shall be passed when fetching access token
--       turns out no IDP enforce this yet

--------------------------------------------------

-- * Services

--------------------------------------------------

exceptToActionM :: ExceptT Text IO a -> ActionM a
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
  token <- withExceptT oauth2ErrorToText $
    ExceptT $ do
      tokenReq idp mgr (ExchangeToken $ TL.toStrict code)
  liftIO $ print token
  (luser, at) <- tryFetchUser mgr token idp
  liftIO $ updateIdp c idpData luser at
  where
    oauth2ErrorToText e =
      TL.pack $
        "tryFetchUser - cannot fetch asses token. error detail: " ++ show e
    updateIdp c1 oldIdpData luser token =
      upsertIDPData
        c1
        (oldIdpData {loginUser = Just luser, oauth2Token = Just token})

tryFetchUser ::
  HasUserReq a =>
  Manager ->
  OAuth2Token ->
  a ->
  ExceptT Text IO (LoginUser, OAuth2Token)
tryFetchUser mgr at idp = do
  user <- withExceptT bslToText $ ExceptT $ userReq idp mgr (accessToken at)
  return (user, at)

doRefreshToken :: IDPData -> ExceptT Text IO OAuth2Token
doRefreshToken (IDPData (IDPApp idp) _ token) = do
  case token of
    Nothing -> throwE "no token found for idp"
    Just at -> case refreshToken at of
      Nothing -> throwE "no refresh token presents. did you add 'offline_access' scope?"
      Just rt -> withExceptT (TL.pack . show) $
        ExceptT $ do
          mgr <- newManager tlsManagerSettings
          tokenRefreshReq idp mgr rt
