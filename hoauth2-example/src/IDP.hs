{-# LANGUAGE OverloadedStrings #-}

module IDP where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified IDP.Auth0 as IAuth0
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
import Network.OAuth.OAuth2
import Session
import System.Directory
import Types

-- TODO:
-- 1. make this generic with discover any IDPs from idp directory.
-- 2. and discover `.env` for override?

createIDPs :: IO [IDPApp]
createIDPs = do
  configParams <- readEnvFile
  let initIdp idp propKey =
        case Aeson.lookup (Aeson.fromString propKey) configParams of
          Nothing -> idp
          Just config ->
            idp
              { oauth2Config =
                  (oauth2Config idp)
                    { oauth2ClientId = clientId config,
                      oauth2ClientSecret = clientSecret config,
                      oauth2RedirectUri = defaultOAuth2RedirectUri
                    },
                oauth2Scopes = maybe [] (map TL.fromStrict) (scopes config)
              }

  return
    [ IDPApp (IAzureAD.AzureAD (initIdp IAzureAD.azureIdp "azure")),
      IDPApp (IOkta.Okta (initIdp IOkta.oktaIdp "okta")),
      IDPApp
        (IAuth0.Auth0 (initIdp IAuth0.auth0Idp "auth0")),
      IDPApp
        (IDouban.Douban (initIdp IDouban.doubanIdp "douban")),
      IDPApp (IDropbox.Dropbox (initIdp IDropbox.dropboxIdp "dropbox")),
      IDPApp (IFacebook.Facebook (initIdp IFacebook.facebookIdp "facebook")),
      IDPApp (IFitbit.Fitbit (initIdp IFitbit.fitbitIdp "fitbit")),
      IDPApp (IGithub.Github (initIdp IGithub.githubIdp "github")),
      IDPApp (IGoogle.Google (initIdp IGoogle.googleIdp "google")),
      IDPApp (ILinkedin.Linkedin (initIdp ILinkedin.linkedinIdp "linkedin")),
      IDPApp (ISlack.Slack (initIdp ISlack.slackIdp "slack")),
      IDPApp (IWeibo.Weibo (initIdp IWeibo.weiboIdp "weibo")),
      IDPApp (IZOHO.ZOHO (initIdp IZOHO.zohoIdp "zoho")),
      IDPApp
        ( IStackExchange.StackExchange
            (initIdp IStackExchange.stackexchangeIdp "stackExchange")
        )
    ]

envFilePath :: String
envFilePath = ".env.json"

readEnvFile :: IO EnvConfig
readEnvFile = do
  envFileE <- doesFileExist envFilePath
  if envFileE
    then do
      putStrLn "Found .env.json"
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
