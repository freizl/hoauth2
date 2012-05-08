{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes  #-}

{-
   templates must follow a kind of scalfold
   check template dir for reference
-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe, fromJust)
import qualified Data.Text as T
import           Network.HTTP.Conduit (responseBody)


#ifdef DEVELOPMENT
import           Snap.Loader.Devel
#else
import           Snap.Loader.Prod
#endif

import           Snap.Core
import           Snap.Http.Server
import           System.IO
import           Snap
import           Snap.Util.FileServe
import           Snap.Snaplet.Heist

import Network.OAuth2.OAuth2
import Network.OAuth2.HTTP.HttpClient

import Key
import Api

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App


------------------------------------------------------------------------------

weibooauth :: OAuth2
weibooauth = weiboKey { oauthOAuthorizeEndpoint = "https://api.weibo.com/oauth2/authorize"
                      , oauthAccessTokenEndpoint = "https://api.weibo.com/oauth2/access_token" 
                      , oauthAccessToken = Nothing
                      }

-- | Login via Weibo. Redirect user for authorization.
weibo :: Handler App App ()
weibo = redirect $ authorizationUrl weibooauth

-- | Callback for oauth provider.
--
-- This function is very annoying....
-- 
oauthCallbackHandler :: Handler App App ()
oauthCallbackHandler = do
  code   <- decodedParam "code"
  token <- liftIO $ requestAccessToken weibooauth code
  case token of 
    Just token' -> do
                   -- getting UID. FIXME: res could be error
                   res <- liftIO $ requestUid accountUidUri token'
                   liftIO $ print $ BS.unpack $ apiUrlGet2 accountShowUri (token', fromJust res)
                   res2 <- liftIO $ doSimpleGetRequest . BS.unpack $ apiUrlGet2 accountShowUri (token', fromJust res)
                   writeLBS $ "user: " `LBS.append` responseBody res2
    _ -> writeBS "Error getting access token"


decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with heist heistServe) -- ^ FIXME: maybe no need heist
          , ("/", writeBS "It works!<a href='/weibo'>test</a>")
          , ("/weibo", weibo)
          , ("/oauthCallback", oauthCallbackHandler )
          ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    addRoutes routes
    return $ App h


------------------------------------------------------------------------------

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf $ site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap ())
getConf = commandLineConfig defaultConfig

getActions :: Config Snap () -> IO (Snap (), IO ())
getActions _ = do
    (msgs, site, cleanup) <- runSnaplet app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
