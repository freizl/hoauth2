{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module App (app, waiApp) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Text.Encoding                as TE
import           Data.Text.Lazy                    (Text)
import qualified Data.Text.Lazy                    as TL
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp          (run)
import           Prelude


import           Data.Maybe
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import qualified Network.Wai                       as WAI
import           Network.Wai.Middleware.Static
import           Web.Scotty
import           Web.Scotty.Internal.Types

import           Api
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
  mapM_ (\idp -> insertKeys c (idpName idp) idp)
  -- TODO: leverage generic ?
  [ mkIDPData Okta
  , mkIDPData Github
  , mkIDPData Google
  , mkIDPData Douban
  , mkIDPData Dropbox
  , mkIDPData Facebook
  , mkIDPData Fitbit
  , mkIDPData Weibo
  , mkIDPData StackExchange
  ]

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
  let maybeIdp = idpFromText (head idpP)
  case maybeIdp of
    Just idpInput -> liftIO (removeKey c idpInput) >> redirectToHomeM
    Nothing       -> redirectToHomeM

indexH :: KeyCache -> ActionM ()
indexH c = do
  is <- liftIO (allValues c)
  overviewTpl is

callbackH :: KeyCache -> ActionM ()
callbackH c = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "ERROR: no code from callback request")
  when (null stateP) (errorM "ERROR: no state from callback request")
  let maybeIdpName = idpFromText $ TL.takeWhile (/= '.') (head stateP)
  case maybeIdpName of
    Just idpN -> fetchTokenAndUser (head codeP) c idpN
    Nothing   -> indexH c         -- TODO: show error message

fetchTokenAndUser :: TL.Text           -- ^ code
                  -> KeyCache
                  -> IDP
                  -> ActionM ()
fetchTokenAndUser code store idpInput = do
  -- TODO: danger using head
  mayBeIdp <- liftIO $ lookupKey store idpInput
  when (isNothing mayBeIdp) (errorM "Error: cannot find idp from session")
  let idpD = fromJust mayBeIdp
  result <- liftIO $ do
    mgr <- newManager tlsManagerSettings
    -- token <- fetchAccessToken mgr (oauth2Key idpD) (ExchangeToken $ TL.toStrict code)
    token <- tryFetchAT idpD mgr (ExchangeToken $ TL.toStrict code)
    print token
    case token of
      Right at -> getUserInfo idpD mgr (accessToken at)
      Left e   -> return (Left $ TL.pack $ show e)
  case result of
    Right lUser -> do
      let newIdp = idpD {loginUser = Just lUser }
      liftIO $ insertKeys store idpInput newIdp
      redirectToHomeM
    Left err -> errorM err

-- Fetch Access Token

tryFetchAT, getAT, postATX, postAT, postAT2 :: IDPData
  -> Manager
  -> ExchangeToken
  -> IO (OAuth2Result TR.Errors OAuth2Token)
tryFetchAT idpD mgr code =
  case idpName idpD of
    Facebook      -> postAT idpD mgr code
    StackExchange -> postAT2 idpD mgr code
    _             -> getAT idpD mgr code

getAT idpD mgr = fetchAccessToken mgr (oauth2Key idpD)

postATX postFn idpD mgr code = do
  let okey = oauth2Key idpD
  let (url, body1) = accessTokenUrl okey code
  let extraBody = authClientBody okey
  postFn mgr okey url (extraBody ++ body1)

postAT idpD mgr code = postATX doJSONPostRequest
postAT2 idpD mgr code = postATX doFlexiblePostRequest

authClientBody okey = [ ("client_id", TE.encodeUtf8 $ oauthClientId okey)
                  , ("client_secret", TE.encodeUtf8 $ oauthClientSecret okey)
                  ]
