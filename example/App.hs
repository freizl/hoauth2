{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module App (app, waiApp) where

import Data.Bifunctor
import           Data.Aeson
import           Data.Aeson.Types
import qualified Control.Applicative                  as CA
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8           as BSL
import qualified Data.HashMap.Strict                  as Map
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as TL
-- import qualified Data.Text                       as T
import qualified Data.Text.Encoding                       as TE
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp             (run)
import           Prelude                              hiding (exp)
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types         as HT
import           GHC.Generics


import           Data.List
import           Data.Maybe
import qualified Network.Wai                          as WAI
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty
import           Web.Scotty.Internal.Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Network.OAuth.OAuth2

import           Cookie
import           Views
import Types
import Utils
import Keys
import Session
import Api

------------------------------
-- App
------------------------------

myServerPort = 9988

app :: IO ()
app = putStrLn ("Starting Server. http://localhost:" ++ (show myServerPort))
         >> waiApp
         >>= run myServerPort

-- TODO: how to add either Monad or a middleware to do session?
waiApp :: IO WAI.Application
waiApp = do
  cache <- initKeyCache
  idps cache
  scottyApp $ do
    middleware $ staticPolicy (mapAssetsDir >-> addBase "dist")
    defaultHandler globalErrorHandler
    get "/" $ indexH cache
    get "/oauth2/callback" $ callbackH cache
    get "/logout" $ logoutH cache


mapAssetsDir :: Policy
mapAssetsDir = policy removeAssetsPrefix
  where removeAssetsPrefix s = stripPrefix "assets/" s CA.<|> Just s

--------------------------------------------------
-- * Handlers
--------------------------------------------------

idps :: KeyCache -> IO ()
idps c =
  mapM_ (\idp -> insertKeys c (idpName idp) idp)
  [ mkIDPData Okta oktaKey oktaCodeUri
  , mkIDPData Github githubKey githubCodeUri
  , mkIDPData Google googleKey googleCodeUri
  , mkIDPData Douban doubanKey doubanCodeUri
  , mkIDPData Dropbox dropboxKey dropboxCodeUri
  , mkIDPData Facebook facebookKey facebookCodeUri
  , mkIDPData Fitbit fitbitKey fitbitCodeUri
  , mkIDPData Weibo weiboKey weiboCodeUri
  , mkIDPData StackExchange stackexchangeKey stackexchangeCodeUri
  ]

oktaCodeUri :: Text
oktaCodeUri = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams [("scope", "openid profile"), ("state", "okta.test-state-123")]
  $ authorizationUrl oktaKey

githubCodeUri :: Text
githubCodeUri = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams [("state", "github.test-state-123")]
  $ authorizationUrl githubKey

googleCodeUri :: Text
googleCodeUri = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams (googleQueryParams)
  $ authorizationUrl googleKey
googleQueryParams :: QueryParams
googleQueryParams = [ ("scope", "https://www.googleapis.com/auth/userinfo.email")
                    , ("state", "google.test-state-123")
                    ]
doubanCodeUri = createCodeUri doubanKey [("state", "douban.test-state-123")]
dropboxCodeUri = createCodeUri dropboxKey [("state", "dropbox.test-state-123")]
facebookCodeUri = createCodeUri facebookKey [ ("state", "facebook.test-state-123")
                                            , ("scope", "user_about_me,email")
                                            ]
fitbitCodeUri = createCodeUri fitbitKey [("state", "fitbit.test-state-123")
                                        , ("scope", "profile")
                                        ]
weiboCodeUri = createCodeUri weiboKey [("state", "weibo.test-state-123")]
stackexchangeCodeUri = createCodeUri stackexchangeKey [("state", "stackexchange.test-state-123")]

createCodeUri key params = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams params
  $ authorizationUrl key

redirectToProfileM :: ActionM ()
redirectToProfileM = redirect "/authorization-code/profile"

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
    Nothing -> redirectToHomeM

indexH :: KeyCache -> ActionM ()
indexH c = do
  is <- liftIO (allValues c)
  overviewTpl is

resultTpl = undefined

callbackH c = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "ERROR: no code from callback request")
  when (null stateP) (errorM "ERROR: no state from callback request")
  let maybeIdpName = idpFromText $ TL.takeWhile (/= '.') (head stateP)
  case maybeIdpName of
    Just idpName -> fetchTokenAndUser (head codeP) c idpName
    Nothing -> resultTpl         -- TODO: show error message

fetchTokenAndUser :: TL.Text           -- ^ code
                  -> KeyCache
                  -> IDP
                  -> ActionM ()
fetchTokenAndUser code store idpInput = do
  -- TODO: danger using head
  mayBeIdp <- liftIO $ lookupKey store idpInput 
  when (isNothing mayBeIdp) (errorM "Error: cannot find idp from session")
  let idpData = fromJust mayBeIdp
  result <- liftIO $ do
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr (oauth2Key idpData) (ExchangeToken $ TL.toStrict code)
    case token of
      Right at -> getUserInfo idpInput mgr (accessToken at)
      Left e -> return (Left $ TL.pack $ show e)
  case result of
    Right lUser -> do
      let newIdp = idpData {loginUser = Just lUser }
      liftIO $ insertKeys store idpInput newIdp
      redirectToHomeM
    Left err -> errorM err

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
