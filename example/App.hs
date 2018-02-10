{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

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
  

------------------------------
-- App
------------------------------

myServerPort = 9988

app :: IO ()
app = putStrLn ("Starting Server. http://localhost:" ++ (show myServerPort))
         >> waiApp
         >>= run myServerPort

waiApp :: IO WAI.Application
waiApp = do
  -- cache <- initKeyCache
  scottyApp $ do
    middleware $ staticPolicy (mapAssetsDir >-> addBase "dist")
    defaultHandler globalErrorHandler
    get "/" $ indexH
    get "/oauth2/callback" $ callbackH
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
  [ mkIDPData Okta oktaKey oktaCodeUri
  , mkIDPData Github githubKey githubCodUri
  ]

githubCodUri = ""

oktaCodeUri :: Text
oktaCodeUri = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams [("scope", "openid profile"), ("state", "okta.test-state-123")]
  $ authorizationUrl oktaKey


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

resultTpl = indexH

callbackH = do
  pas <- params
  let codeP = paramValue "code" pas
  let stateP = paramValue "state" pas
  when (null codeP) (errorM "ERROR: no code from callback request")
  when (null stateP) (errorM "ERROR: no state from callback request")
  let maybeIdpName = idpFromText $ TL.takeWhile (/= '.') (head stateP)
  case maybeIdpName of
    Just idpName -> fetchTokenAndUser (head codeP) idpName
    Nothing -> resultTpl         -- TODO: show error message

fetchTokenAndUser :: TL.Text           -- ^ code
                  -> IDP
                  -> ActionM ()
fetchTokenAndUser code idpInput = do
  -- TODO: danger using head
  let idp = head $ filter (\idpO -> (idpName idpO) == idpInput) idps
  result <- liftIO $ do
    mgr <- newManager tlsManagerSettings
    token <- fetchAccessToken mgr (oauth2Key idp) (ExchangeToken $ TL.toStrict code)
    case token of
      Right at -> getUserInfo mgr (accessToken at)
      Left e -> return (Left $ TL.pack $ show e)
  case result of
    Right oUser -> overviewTpl [ idp {loginUser = Just (LoginUser $ name oUser) } ]
    Left err -> errorM err

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

data OktaUser = OktaUser { name :: Text }
  deriving (Show, Generic)

instance FromJSON OktaUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON OktaUser where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = camelTo2 '_' }


getUserInfo :: Manager -> AccessToken -> IO (Either Text OktaUser)
getUserInfo mgr token = do
  re <- authGetJSON mgr token [uri|https://dev-148986.oktapreview.com/oauth2/v1/userinfo|]
  return (first (TL.pack . show) ( re :: OAuth2Result Errors OktaUser ))

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
