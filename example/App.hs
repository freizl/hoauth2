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
  let maybeIdp = parseIDP (head idpP)
  case maybeIdp of
    Right (IDPApp idp) -> liftIO (removeKey c (idpLabel idp)) >> redirectToHomeM
    Left e       -> errorM ("logout: unknown IDP " `TL.append` e)

indexH :: CacheStore -> ActionM ()
indexH c = do
  is <- liftIO (allValues c)
  overviewTpl is

callbackH :: CacheStore -> ActionM ()
callbackH c = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "callbackH: no code from callback request")
  when (null stateP) (errorM "callbackH: no state from callback request")
  let maybeIdpName = parseIDP (TL.takeWhile (/= '.') (head stateP))
  -- TODO: looks like `state` shall be passed when fetching access token
  --       turns out no IDP enforce this yet
  case maybeIdpName of
    Right (IDPApp idp) -> fetchTokenAndUser c (head codeP) idp
    Left e   -> errorM ("callbackH: cannot find IDP name from text " `TL.append` e)

fetchTokenAndUser :: (HasTokenReq a, HasUserReq a, HasLabel a) => CacheStore
                  -> TL.Text           -- ^ code
                  -> a
                  -> ActionM ()
fetchTokenAndUser store code idpInput = do
  mayBeIdp <- liftIO $ lookupKey store (idpLabel idpInput)
  when (isNothing mayBeIdp) (errorM "fetchTokenAndUser: cannot find idp data from cache")
  let idpD = fromJust mayBeIdp
  result <- liftIO $ do
    mgr <- newManager tlsManagerSettings
    token <- tokenReq idpInput mgr (ExchangeToken $ TL.toStrict code)
    when debug (print token)
    case token of
      Right at -> tryFetchUser idpInput mgr (accessToken at)
      Left e   -> return (Left $ TL.pack $ "cannot fetch asses token. error detail: " ++ show e)
  case result of
    Right lUser -> do
      let newIdp = idpD {loginUser = Just lUser }
      liftIO $ insertIDPData store newIdp
      redirectToHomeM
    Left err -> errorM ("fetchTokenAndUser: " `TL.append` err)

-- * Fetch UserInfo
--
tryFetchUser :: (HasUserReq a) => a -> Manager -> AccessToken -> IO (Either Text LoginUser)
tryFetchUser idp mgr token = do
  re <- userReq idp mgr token
  return (first displayOAuth2Error re)

displayOAuth2Error :: OAuth2Error Errors -> Text
displayOAuth2Error = TL.pack . show

