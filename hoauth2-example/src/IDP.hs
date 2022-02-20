{-# LANGUAGE QuasiQuotes #-}

module IDP where

import URI.ByteString
import URI.ByteString.QQ
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString as BS
-- import qualified IDP.Auth0 as IAuth0
import qualified IDP.AzureAD as IAzureAD
import qualified IDP.Douban as IDouban
import qualified IDP.Dropbox as IDropbox
import qualified IDP.Facebook as IFacebook
import qualified IDP.Fitbit as IFitbit
import qualified IDP.Github as IGithub
import qualified IDP.Google as IGoogle
import qualified IDP.Linkedin as ILinkedin
import qualified IDP.Okta as IOkta
import qualified IDP.Slack as ISlack
import qualified IDP.StackExchange as IStackExchange
import qualified IDP.Weibo as IWeibo
import qualified IDP.ZOHO as IZOHO
import Keys
import Network.OAuth.OAuth2
import Session
import System.Directory
import Types

-- TODO:
-- 1. make this generic to discover any IDPs from idp directory.
-- 2. and discover `.env` for override?

createIDPs :: IO [IDPApp]
createIDPs = do
  configCreds <- readEnvFile
  let initKey preConfigOAuth envKey =
        let oauthKey = case Aeson.lookup (Aeson.fromString envKey) configCreds of
              Nothing -> preConfigOAuth
              Just config ->
                preConfigOAuth
                  { oauth2ClientId = clientId config,
                    oauth2ClientSecret = clientSecret config
                  }
         in oauthKey {oauth2RedirectUri = defaultOAuth2RedirectUri}

  return
    [ IDPApp (IAzureAD.init (initKey IAzureAD.azureADKey "azure")),
      IDPApp (IOkta.init (initKey IOkta.oktaKey "okta"))
      -- IDPApp (IAuth0.Auth0 (initKey auth0Key "auth0")),
      -- IDPApp (IDouban.Douban (initKey doubanKey "douban")),
      -- IDPApp (IDropbox.Dropbox (initKey dropboxKey "dropbox")),
      -- IDPApp (IFacebook.Facebook (initKey facebookKey "facebook")),
      -- IDPApp (IFitbit.Fitbit (initKey fitbitKey "fitbit")),
      -- IDPApp (IGithub.Github (initKey githubKey "github")),
      -- IDPApp (IGoogle.Google (initKey googleKey "google")),
      -- IDPApp (ILinkedin.Linkedin (initKey linkedinKey "linkedin")),
      -- IDPApp (ISlack.Slack (initKey slackKey "slack")),
      -- IDPApp (IWeibo.Weibo (initKey weiboKey "weibo")),
      -- IDPApp (IZOHO.ZOHO (initKey zohoKey "zoho")),
      -- IDPApp
      --   ( IStackExchange.StackExchange
      --       (initKey stackexchangeKey "stackExchange")
      --       stackexchangeAppKey
      --   )
    ]

defaultOAuth2RedirectUri :: Maybe URI
defaultOAuth2RedirectUri = Just [uri|http://localhost:9988/oauth2/callback|]

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: IO EnvConfig
readEnvFile = do
  envFileE <- doesFileExist envFilePath
  if envFileE
    then do
      print "Found .env.json"
      fileContent <- BS.readFile envFilePath
      case Aeson.eitherDecodeStrict fileContent of
        Left err -> print err >> return Aeson.empty
        Right ec -> return ec
    else return Aeson.empty

initIdps :: CacheStore -> IO ()
initIdps c = do
  idps <- createIDPs
  mapM_ (upsertIDPData c) (fmap mkIDPData idps)

mkIDPData :: IDPApp -> IDPData
mkIDPData ia = IDPData ia Nothing Nothing
