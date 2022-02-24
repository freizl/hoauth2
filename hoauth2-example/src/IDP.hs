{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  let initIdp idp =
        case Aeson.lookup (Aeson.fromString $ TL.unpack $ TL.toLower $ getIdpName idp) configParams of
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
    [ IDPApp (initIdp IAzureAD.azureIdp),
      IDPApp
        (initIdp IAuth0.auth0Idp),
      IDPApp
        (initIdp IDouban.doubanIdp),
      IDPApp (initIdp IDropbox.dropboxIdp),
      IDPApp (initIdp IFacebook.facebookIdp),
      IDPApp (initIdp IFitbit.fitbitIdp),
      IDPApp (initIdp IGithub.githubIdp),
      IDPApp (initIdp IGoogle.googleIdp),
      IDPApp (initIdp ILinkedin.linkedinIdp),
      IDPApp
        (initIdp IOkta.oktaIdp),
      IDPApp
        (initIdp ISlack.slackIdp),
      IDPApp (initIdp IWeibo.weiboIdp),
      IDPApp (initIdp IZOHO.zohoIdp),
      IDPApp
        (initIdp IStackExchange.stackexchangeIdp)
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
