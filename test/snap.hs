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
--import qualified Data.ByteString.Char8 as BS

--import           Text.Templating.Heist
--import           Text.XmlHtml hiding (render)
--import qualified Text.XmlHtml as X
--import           Control.Applicative
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
--import qualified Data.Text.Encoding as T

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

------------------------------------------------------------------------------

data App = App
    { _heist   :: Snaplet (Heist App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App


------------------------------------------------------------------------------
  
oauthCallback :: Handler App App ()
oauthCallback = do
  --adir   <- decodedParam "dir"
  writeBS "oauthCallback"  
  --where
    --mdSplices :: ByteString -> Splice AppHandler
    --mdSplices p = do return $ [TextNode $ T.decodeUtf8 (BS.concat [p,".md"]) ]
  

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p




------------------------------------------------------------------------------

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with heist heistServe) -- ^ FIXME: maybe no heist
          , ("", serveDirectory "static")
          , ("/", writeBS "It works!")
          , ("/weiboCallback", oauthCallback )
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
