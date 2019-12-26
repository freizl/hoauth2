
module IDP where

import           Data.Text.Lazy      (Text)

import qualified Data.HashMap.Strict as Map
import qualified IDP.AzureAD         as IAzureAD
import qualified IDP.Douban          as IDouban
import qualified IDP.Dropbox         as IDropbox
import qualified IDP.Facebook        as IFacebook
import qualified IDP.Fitbit          as IFitbit
import qualified IDP.Github          as IGithub
import qualified IDP.Google          as IGoogle
import qualified IDP.Okta            as IOkta
import qualified IDP.ZOHO            as IZOHO
import qualified IDP.StackExchange   as IStackExchange
import qualified IDP.Weibo           as IWeibo
import           Session
import           Types

-- TODO: make this generic to discover any IDPs from idp directory.
--
idps :: [IDPApp]
idps = [ IDPApp IAzureAD.AzureAD
       , IDPApp IDouban.Douban
       , IDPApp IDropbox.Dropbox
       , IDPApp IFacebook.Facebook
       , IDPApp IFitbit.Fitbit
       , IDPApp IGithub.Github
       , IDPApp IGoogle.Google
       , IDPApp IOkta.Okta
       , IDPApp IStackExchange.StackExchange
       , IDPApp IWeibo.Weibo
       , IDPApp IZOHO.ZOHO
       ]

initIdps :: CacheStore -> IO ()
initIdps c = mapM_ (insertIDPData c) (fmap mkIDPData idps)

idpsMap :: Map.HashMap Text IDPApp
idpsMap = Map.fromList $ fmap (\x@(IDPApp idp) -> (idpLabel idp, x)) idps

parseIDP :: Text -> Either Text IDPApp
parseIDP s = maybe (Left s) Right (Map.lookup s idpsMap)

mkIDPData :: IDPApp -> IDPData
mkIDPData (IDPApp idp) = IDPData (authUri idp) Nothing Nothing (idpLabel idp)
