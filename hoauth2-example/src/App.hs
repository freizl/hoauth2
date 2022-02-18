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
import Control.Monad.IO.Class
  ( liftIO,
  )
import Control.Monad.Trans.Except
import Data.Bifunctor
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
    middleware $ staticPolicy (addBase "example/assets")
    defaultHandler globalErrorHandler
    get "/" $ indexH cache
    get "/oauth2/callback" $ callbackH cache
    get "/logout" $ logoutH cache
    get "/refresh" $ refreshH cache

debug :: Bool
debug = True

--------------------------------------------------

-- * Handlers

--------------------------------------------------

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status401 >> html t

readIdpParam :: CacheStore -> ActionM (Either IDPLabel IDPData)
readIdpParam c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  liftIO $ lookupKey c (head idpP)

refreshH :: CacheStore -> ActionM ()
refreshH c = do
  eitherIdpApp <- readIdpParam c
  case eitherIdpApp of
    Right idpData -> do
      nt <- liftIO $ runExceptT (doRefreshToken idpData)
      case nt of
        Left e -> raise (TL.pack e)
        Right newToken -> do
          liftIO $ putStrLn "got new token"
          liftIO $ print newToken
          liftIO $ upsertIDPData c (idpData {oauth2Token = Just newToken})
          redirectToHomeM
    Left e -> raise ("refreshH: unknown IDP " `TL.append` e)

doRefreshToken :: IDPData -> ExceptT String IO OAuth2Token
doRefreshToken (IDPData (IDPApp idp) _ token) = do
  mgr <- liftIO $ newManager tlsManagerSettings
  case token of
    Nothing -> throwE "no token found for idp"
    Just at -> case refreshToken at of
      Nothing -> throwE "no refresh token presents. did you add 'offline_access' scope?"
      Just rt -> withExceptT show $ ExceptT $ tokenRefreshReq idp mgr rt

logoutH :: CacheStore -> ActionM ()
logoutH c = do
  eitherIdpApp <- readIdpParam c
  case eitherIdpApp of
    Right idpData ->
      liftIO (removeKey c (toLabel idpData)) >> redirectToHomeM
    Left e -> raise ("logout: unknown IDP " `TL.append` e)

indexH :: CacheStore -> ActionM ()
indexH c = liftIO (allValues c) >>= overviewTpl

callbackH :: CacheStore -> ActionM ()
callbackH c = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (raise "callbackH: no code from callback request")
  when (null stateP) (raise "callbackH: no state from callback request")
  eitherIdpData <- liftIO $ lookupKey c (TL.takeWhile (/= '.') (head stateP))

  -- TODO: looks like `state` shall be passed when fetching access token
  --       turns out no IDP enforce this yet
  case eitherIdpData of
    Right idpData -> fetchTokenAndUser c (head codeP) idpData
    Left e ->
      raise ("callbackH: cannot find IDP name from text " `TL.append` e)

fetchTokenAndUser ::
  CacheStore ->
  -- | code
  TL.Text ->
  IDPData ->
  ActionM ()
fetchTokenAndUser c code idpData = do
  result <- liftIO $ runExceptT $ fetchTokenAndUser' c code idpData
  case result of
    Right _ -> redirectToHomeM
    Left err -> raise err

fetchTokenAndUser' ::
  CacheStore ->
  Text ->
  IDPData ->
  ExceptT Text IO ()
fetchTokenAndUser' c code idpData@(IDPData (IDPApp idp) _ _) = do
  mgr <- liftIO $ newManager tlsManagerSettings
  token <- withExceptT oauth2ErrorToText $
    ExceptT $ tokenReq idp mgr (ExchangeToken $ TL.toStrict code)
  when debug (liftIO $ print token)

  (luser, at) <- tryFetchUser mgr token idp
  -- result <- case token of
  --   Right at ->
  --   Left e ->
  --     return
  --       ( Left $
  --           TL.pack $
  --             "tryFetchUser - cannot fetch asses token. error detail: "
  --               ++ show e
  --       )

  liftIO $ updateIdp c idpData luser at
  -- case result of
  --   Right (luser, at) ->
  --   Left err ->
  --     return $ Left ("fetchTokenAndUser - no user found: " `TL.append` err)
  where
    oauth2ErrorToText e = TL.pack $
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
  user' <- fetchUser idp mgr (accessToken at)
  return (user', at)

-- * Fetch UserInfo

--
fetchUser ::
  (HasUserReq a) => a -> Manager -> AccessToken -> ExceptT Text IO LoginUser
fetchUser idp mgr token = ExceptT $ do
  re <- userReq idp mgr token
  return (first bslToText re)
