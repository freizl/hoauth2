module IDP where

import qualified Data.HashMap.Strict as Map
import Data.Text.Lazy (Text)
import qualified IDP.Auth0 as IAuth0
import qualified IDP.AzureAD as IAzureAD
import qualified IDP.Douban as IDouban
import qualified IDP.Dropbox as IDropbox
import qualified IDP.Facebook as IFacebook
import qualified IDP.Fitbit as IFitbit
import qualified IDP.Github as IGithub
import qualified IDP.Google as IGoogle
import qualified IDP.Okta as IOkta
import qualified IDP.Slack as ISlack
import qualified IDP.StackExchange as IStackExchange
import qualified IDP.Weibo as IWeibo
import qualified IDP.ZOHO as IZOHO
import Keys
import Session
import Types

-- TODO: make this generic to discover any IDPs from idp directory.
--
createIDPs :: IO [IDPApp]
createIDPs =
  return [ IDPApp (IAzureAD.AzureAD azureADKey),
    IDPApp (IDouban.Douban doubanKey),
    IDPApp (IDropbox.Dropbox dropboxKey),
    IDPApp (IFacebook.Facebook facebookKey),
    IDPApp (IFitbit.Fitbit fitbitKey),
    IDPApp (IGithub.Github githubKey),
    IDPApp (IGoogle.Google googleKey),
    IDPApp (IOkta.Okta oktaKey),
    IDPApp (IStackExchange.StackExchange stackexchangeKey stackexchangeAppKey),
    IDPApp (IWeibo.Weibo weiboKey),
    IDPApp (IAuth0.Auth0 auth0Key),
    IDPApp (ISlack.Slack slackKey),
    IDPApp (IZOHO.ZOHO zohoKey)
  ]

initIdps :: CacheStore -> IO ()
initIdps c = do
  idps <- createIDPs
  mapM_ (insertIDPData c) (fmap mkIDPData idps)

idpsMap :: Map.HashMap Text IDPApp
idpsMap = Map.fromList $ fmap (\x@(IDPApp idp) -> (idpLabel idp, x)) idps

parseIDP :: Text -> Either Text IDPApp
parseIDP s = maybe (Left s) Right (Map.lookup s idpsMap)

mkIDPData :: IDPApp -> IDPData
mkIDPData (IDPApp idp) = IDPData (authUri idp) Nothing Nothing (idpLabel idp)
