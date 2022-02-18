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
  -- return $ parseIDP (head idpP)
  liftIO $ lookupKey c (head idpP)

refreshH :: CacheStore -> ActionM ()
refreshH c = do
  eitherIdpApp <- readIdpParam c
  case eitherIdpApp of
    Right idpData -> do
      -- maybeIdpData <- lookIdp c idp
      -- when (isNothing maybeIdpData)
      --      (raise "refreshH: cannot find idp data from cache")
      -- let idpData = fromJust maybeIdpData
      re <- liftIO $ doRefreshToken idpData
      case re of
        Right newToken -> liftIO (print newToken) >> redirectToHomeM -- TODO: update access token in the store
        Left e -> raise (TL.pack e)
    Left e -> raise ("refreshH: unknown IDP " `TL.append` e)

doRefreshToken :: IDPData -> IO (Either String OAuth2Token)
doRefreshToken (IDPData (IDPApp idp) _ token) = do
  mgr <- newManager tlsManagerSettings
  case token of
    Nothing -> return $ Left "no token found for idp"
    Just at -> case refreshToken at of
      Nothing -> return $ Left "no refresh token presents"
      Just rt -> do
        re <- tokenRefreshReq idp mgr rt
        return (first show re)

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
  -- let eitherIdpApp = parseIDP (TL.takeWhile (/= '.') (head stateP))
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
  -- maybeIdpData <- lookIdp c idp
  -- when (isNothing maybeIdpData)
  --      (raise "fetchTokenAndUser - cannot find idp data from cache")
  -- let idpData = fromJust maybeIdpData
  result <- liftIO $ fetchTokenAndUser' c code idpData
  case result of
    Right _ -> redirectToHomeM
    Left err -> raise err

fetchTokenAndUser' ::
  CacheStore ->
  Text ->
  IDPData ->
  IO (Either Text ())
fetchTokenAndUser' c code idpData@(IDPData (IDPApp idp) _ _) = do
  mgr <- newManager tlsManagerSettings
  token <- tokenReq idp mgr (ExchangeToken $ TL.toStrict code)
  when debug (print token)

  result <- case token of
    Right at -> tryFetchUser mgr at idp
    Left e ->
      return
        ( Left $
            TL.pack $
              "tryFetchUser - cannot fetch asses token. error detail: "
                ++ show e
        )

  case result of
    Right (luser, at) -> updateIdp c idpData luser at >> return (Right ())
    Left err ->
      return $ Left ("fetchTokenAndUser - no user found: " `TL.append` err)
  where
    updateIdp c1 oldIdpData luser token =
      insertIDPData
        c1
        (oldIdpData {loginUser = Just luser, oauth2Token = Just token})

-- lookIdp :: (MonadIO m, HasLabel a) => CacheStore -> a -> m (Either IDPLabel IDPData)
-- lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)

-- TODO: may use Exception monad to capture error in this IO monad
--
tryFetchUser ::
  HasUserReq a =>
  Manager ->
  OAuth2Token ->
  a ->
  IO (Either Text (LoginUser, OAuth2Token))
tryFetchUser mgr at idp = do
  re <- fetchUser idp mgr (accessToken at)
  return $ case re of
    Right user' -> Right (user', at)
    Left e -> Left e

-- * Fetch UserInfo

--
fetchUser ::
  (HasUserReq a) => a -> Manager -> AccessToken -> IO (Either Text LoginUser)
fetchUser idp mgr token = do
  re <- userReq idp mgr token
  return (first bslToText re)
