{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module App (app, waiApp) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as TL
import           IDP
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.OAuth.OAuth2
import qualified Network.Wai                   as WAI
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Static
import           Prelude
import           Session
import           Types
import           Utils
import           Views
import           Web.Scotty
import           Web.Scotty.Internal.Types

------------------------------
-- App
------------------------------

myServerPort :: Int
myServerPort = 9988

app :: IO ()
app = putStrLn ("Starting Server. http://localhost:" ++ show myServerPort)
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

errorM :: Text -> ActionM ()
errorM = throwError . ActionError

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status401 >> html t

readIdpParam :: ActionM (Either Text IDPApp)
readIdpParam = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  return $ parseIDP (head idpP)

refreshH :: CacheStore -> ActionM ()
refreshH c = do
  eitherIdpApp <- readIdpParam
  case eitherIdpApp of
    Right (IDPApp idp) -> do
      maybeIdpData <- lookIdp c idp
      when (isNothing maybeIdpData) (errorM "refreshH: cannot find idp data from cache")
      let idpData = fromJust maybeIdpData
      re <- liftIO $ doRefreshToken idp idpData
      case re of
        Right newToken -> liftIO (print newToken) >> redirectToHomeM -- TODO: update access token in the store
        Left e         -> errorM (TL.pack e)
    Left e       -> errorM ("logout: unknown IDP " `TL.append` e)

doRefreshToken :: HasTokenRefreshReq a =>
                  a -> IDPData -> IO (Either String OAuth2Token)
doRefreshToken idp idpData = do
  mgr <- newManager tlsManagerSettings
  case oauth2Token idpData of
    Nothing -> return $ Left "no token found for idp"
    Just at ->
      case refreshToken at of
        Nothing -> return $ Left "no refresh token presents"
        Just rt -> do
          re <- tokenRefreshReq idp mgr rt
          return (first show re)

logoutH :: CacheStore -> ActionM ()
logoutH c = do
  eitherIdpApp <- readIdpParam
  -- let eitherIdpApp = parseIDP (head idpP)
  case eitherIdpApp of
    Right (IDPApp idp) -> liftIO (removeKey c (idpLabel idp)) >> redirectToHomeM
    Left e       -> errorM ("logout: unknown IDP " `TL.append` e)

indexH :: CacheStore -> ActionM ()
indexH c = liftIO (allValues c) >>= overviewTpl

callbackH :: CacheStore -> ActionM ()
callbackH c = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "callbackH: no code from callback request")
  when (null stateP) (errorM "callbackH: no state from callback request")
  let eitherIdpApp = parseIDP (TL.takeWhile (/= '.') (head stateP))
  -- TODO: looks like `state` shall be passed when fetching access token
  --       turns out no IDP enforce this yet
  case eitherIdpApp of
    Right (IDPApp idp) -> fetchTokenAndUser c (head codeP) idp
    Left e   -> errorM ("callbackH: cannot find IDP name from text " `TL.append` e)

fetchTokenAndUser :: (HasTokenReq a, HasUserReq a, HasLabel a)
                  => CacheStore
                  -> TL.Text           -- ^ code
                  -> a
                  -> ActionM ()
fetchTokenAndUser c code idp = do
  maybeIdpData <- lookIdp c idp
  when (isNothing maybeIdpData) (errorM "fetchTokenAndUser: cannot find idp data from cache")

  let idpData = fromJust maybeIdpData
  result <- liftIO $ fetchTokenAndUser' c code idp idpData
  case result of
    Right _  -> redirectToHomeM
    Left err -> errorM err

fetchTokenAndUser' :: (HasTokenReq a, HasUserReq a) =>
                      CacheStore -> Text -> a -> IDPData -> IO (Either Text ())
fetchTokenAndUser' c code idp idpData = do
  mgr <- newManager tlsManagerSettings
  token <- tokenReq idp mgr (ExchangeToken $ TL.toStrict code)
  when debug (print token)

  result <- case token of
    Right at -> tryFetchUser mgr at idp
    Left e   -> return (Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e)

  case result of
    Right (luser, at) -> updateIdp c idpData luser at >> return (Right ())
    Left err    -> return $ Left ("fetchTokenAndUser: " `TL.append` err)

  where updateIdp c1 oldIdpData luser token =
          insertIDPData c1 (oldIdpData {loginUser = Just luser, oauth2Token = Just token })

lookIdp :: (MonadIO m, HasLabel a) =>
           CacheStore -> a -> m (Maybe IDPData)
lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)

-- TODO: may use Exception monad to capture error in this IO monad
--
tryFetchUser :: HasUserReq a =>
                Manager
             -> OAuth2Token -> a -> IO (Either Text (LoginUser, OAuth2Token))
tryFetchUser mgr at idp = do
  re <- fetchUser idp mgr (accessToken at)
  return $ case re of
    Right user' -> Right (user', at)
    Left e      -> Left e

-- * Fetch UserInfo
--
fetchUser :: (HasUserReq a) => a -> Manager -> AccessToken -> IO (Either Text LoginUser)
fetchUser idp mgr token = do
  re <- userReq idp mgr token
  return (first bslToText re)
