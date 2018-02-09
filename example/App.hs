{-# LANGUAGE OverloadedStrings #-}

module App (app, waiApp) where

import qualified Control.Applicative                  as CA
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Lazy.Char8           as BS
import qualified Data.HashMap.Strict                  as Map
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as T
--import           Data.Time.Format
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp             (run)
import           Prelude                              hiding (exp)

import           Data.List
import           Data.Maybe
import qualified Network.Wai                          as WAI
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty
import           Web.Scotty.Internal.Types

import           Cookie
import           Views
import Types

------------------------------
-- App
------------------------------

port = 9988

app :: IO ()
app = putStrLn ("Starting Server at " ++ (show port))
         >> waiApp
         >>= run port

waiApp :: IO WAI.Application
waiApp = do
  -- cache <- initKeyCache
  scottyApp $ do
    middleware $ staticPolicy (mapAssetsDir >-> addBase "dist")
    defaultHandler globalErrorHandler
    get "/" $ indexH
    --get "/authorization-code/login-redirect" $ loginRedirectH c
    --get "/authorization-code/login-custom" $ loginCustomH c
    --get "/authorization-code/profile" $ profileH c
    --get "/authorization-code/logout" logoutH
    --get "/authorization-code/callback" $ callbackH cache c


mapAssetsDir :: Policy
mapAssetsDir = policy removeAssetsPrefix
  where removeAssetsPrefix s = stripPrefix "assets/" s CA.<|> Just s

--------------------------------------------------
-- * Handlers
--------------------------------------------------

idps :: [IDPData]
idps =
  [ IDPData "/abc" False Nothing Okta
  , IDPData "http://www.github.com/auth" False Nothing Github
  ]

redirectToProfileM :: ActionM ()
redirectToProfileM = redirect "/authorization-code/profile"

redirectToHomeM :: ActionM ()
redirectToHomeM = redirect "/"

errorM :: Text -> ActionM ()
errorM = throwError . ActionError

globalErrorHandler :: Text -> ActionM ()
globalErrorHandler t = status status401 >> html t

indexH :: ActionM ()
indexH = overviewTpl idps

{-
loginRedirectH :: Config -> ActionM ()
loginRedirectH c = withCookieUserM (const redirectToProfileM) (loginRedirectTpl c)

loginCustomH :: Config -> ActionM ()
loginCustomH c = withCookieUserM (const redirectToProfileM) (loginCustomTpl c)

logoutH :: ActionM ()
logoutH = deleteCookieUserM >> redirectToHomeM

callbackH :: KeyCache -> Config -> ActionM ()
callbackH cache c = do
  -- params from cookie
  stateC <- maybeToList <$> getCookiesM "okta-oauth-state"
  nonceC <- maybeToList <$> getCookiesM "okta-oauth-nonce"

  -- params from callback request query
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  let errorP = paramValue "error" pas
  let errorDescP = paramValue "error_description" pas

  -- validation failure hence error flow
  unless (null errorP) (errorM $ T.unwords $ errorP ++ [":"] ++ errorDescP)
  when (null codeP) (errorM "no code found from callback request")
  when (null stateP) (errorM "no state found from callback request")
  when (null nonceC) (errorM "no nonce found in the cookie")
  when (null stateC) (errorM "no state found in the cookie")
  when (stateP /= stateC) (errorM $
                           T.unlines $
                           ["state is not match: "] ++
                           [T.unwords $ "state parameter:": stateP] ++
                           [T.unwords $ "state cookie:": stateC]
                          )
  -- successful flow
  return ()
-}
