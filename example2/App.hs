{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module App (app, waiApp) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class            (liftIO)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as TL
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import qualified Network.Wai                       as WAI
import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.Static
import           Prelude
import           Web.Scotty
import           Web.Scotty.Internal.Types

import           Session
import           Types
import           Utils
import           Views

import qualified IDP.Github           as IGithub
import qualified IDP.Facebook         as IFacebook


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
  cache <- initKeyCache
  idps cache
  scottyApp $ do
    middleware $ staticPolicy (addBase "example/assets")
    defaultHandler globalErrorHandler
    get "/" $ indexH cache
    get "/oauth2/callback" $ callbackH cache
    get "/logout" $ logoutH cache


--------------------------------------------------
-- * Handlers
--------------------------------------------------

idps :: KeyCache -> IO ()
idps c =
  mapM_ (\idp -> insertKeys c idp)
  [ mkIDPData IGithub.Github
  , mkIDPData IFacebook.Facebook
  ]
  
mkIDPData :: (HasAuthUri a, IDPLabel a) => a -> IDPData
mkIDPData idp = IDPData (authUri idp) Nothing (idpLabel idp)

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

errorM :: Text -> ActionM ()
errorM = throwError . ActionError

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status401 >> html t

logoutH :: KeyCache -> ActionM ()
logoutH c = do
  pas <- params
  let idpP = paramValue "idp" pas
  when (null idpP) redirectToHomeM
  let maybeIdp = (head idpP)
  case maybeIdp of
    "Github" -> liftIO (removeKey c IGithub.Github) >> redirectToHomeM
    "Facebook" -> liftIO (removeKey c IFacebook.Facebook) >> redirectToHomeM
    _       -> errorM ("logout: unknown IDP " `TL.append` head idpP)

indexH :: KeyCache -> ActionM ()
indexH c = do
  is <- liftIO (allValues c)
  overviewTpl is

callbackH :: KeyCache -> ActionM ()
callbackH c = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "callbackH: no code from callback request")
  when (null stateP) (errorM "callbackH: no state from callback request")
  let maybeIdpName = TL.takeWhile (/= '.') (head stateP)
  -- TODO: looks like `state` shall be passed when fetching access token
  --       turns out no IDP enforce this yet
  case maybeIdpName of
    "Github" -> fetchTokenAndUser c (head codeP) IGithub.Github
    "Facebook" -> fetchTokenAndUser c (head codeP) IFacebook.Facebook
    _   -> errorM ("callbackH: cannot find IDP name from text " `TL.append` head stateP)

fetchTokenAndUser :: (HasTokenReq a, HasUserReq a, IDPLabel a) => KeyCache
                  -> TL.Text           -- ^ code
                  -> a
                  -> ActionM ()
fetchTokenAndUser store code idpInput = do
  mayBeIdp <- liftIO $ lookupKey store idpInput
  when (isNothing mayBeIdp) (errorM "fetchTokenAndUser: cannot find idp data from cache")
  let idpD = fromJust mayBeIdp
  result <- liftIO $ do
    mgr <- newManager tlsManagerSettings
    token <- tryFetchAT idpInput mgr (ExchangeToken $ TL.toStrict code)
    --print token
    case token of
      Right at -> tryFetchUser idpInput mgr (accessToken at)
      Left e   -> return (Left $ TL.pack $ "cannot fetch asses token. error detail: " ++ show e)
  case result of
    Right lUser -> do
      let newIdp = idpD {loginUser = Just lUser }
      liftIO $ insertKeys store newIdp
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

-- * Fetch Access Token
--
tryFetchAT :: (HasTokenReq a) => a
  -> Manager
  -> ExchangeToken
  -> IO (OAuth2Result TR.Errors OAuth2Token)
tryFetchAT idp mgr code = tokenReq idp mgr code
