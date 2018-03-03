{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

module App (app, waiApp) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as TL
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.OAuth.OAuth2
import qualified Network.Wai                   as WAI
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Static
import           Prelude
import           Web.Scotty
import           Web.Scotty.Internal.Types

import           IDP
import           Session
import           Types
import           Utils
import           Views

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

logoutH :: CacheStore -> ActionM ()
logoutH c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  let eitherIdpApp = parseIDP (head idpP)
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
  result <- liftIO $ tryFetchUser idp code

  case result of
    Right luser -> updateIdp c idpData luser >> redirectToHomeM
    Left err    -> errorM ("fetchTokenAndUser: " `TL.append` err)

  where lookIdp c1 idp1 = liftIO $ lookupKey c1 (idpLabel idp1)
        updateIdp c1 oldIdpData luser = liftIO $ insertIDPData c1 (oldIdpData {loginUser = Just luser })

-- TODO: may use Exception monad to capture error in this IO monad
--
tryFetchUser :: (HasTokenReq a, HasUserReq a, HasLabel a)
  => a
  -> TL.Text           -- ^ code
  -> IO (Either Text LoginUser)
tryFetchUser idp code = do
  mgr <- newManager tlsManagerSettings
  token <- tokenReq idp mgr (ExchangeToken $ TL.toStrict code)
  when debug (print token)
  case token of
    Right at -> fetchUser idp mgr (accessToken at)
    Left e   -> return (Left $ TL.pack $ "tryFetchUser: cannot fetch asses token. error detail: " ++ show e)

-- * Fetch UserInfo
--
fetchUser :: (HasUserReq a) => a -> Manager -> AccessToken -> IO (Either Text LoginUser)
fetchUser idp mgr token = do
  re <- userReq idp mgr token
  return (first displayOAuth2Error re)

displayOAuth2Error :: OAuth2Error Errors -> Text
displayOAuth2Error = TL.pack . show

