{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api where

import Data.Bifunctor
import           Data.Aeson
import           Data.Aeson.Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Network.OAuth.OAuth2
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Encoding                       as TE
import           Network.HTTP.Conduit
import           GHC.Generics

import Keys
import Types
import qualified IDP.Okta as IOkta
import qualified IDP.Github as IGithub
import qualified IDP.Douban as IDouban
import qualified IDP.Dropbox as IDropbox
import qualified IDP.Facebook as IFacebook
import qualified IDP.Fitbit as IFitbit
import qualified IDP.Google as IGoogle
import qualified IDP.StackExchange as IStackExchange
import qualified IDP.Weibo as IWeibo

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

createCodeUri key params = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams params
  $ authorizationUrl key

mkIDPData :: IDP -> IDPData
mkIDPData Okta =
  let uri = createCodeUri oktaKey [("scope", "openid profile"), ("state", "okta.test-state-123")]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Okta
          , oauth2Key = oktaKey
          , userApiUri = IOkta.userInfoUri
          , toLoginUser = IOkta.toLoginUser
          }
mkIDPData Douban =
  let uri = createCodeUri doubanKey [("state", "douban.test-state-123")]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Douban
          , oauth2Key = doubanKey
          , userApiUri = IDouban.userInfoUri
          , toLoginUser = IDouban.toLoginUser
          }
mkIDPData Dropbox =
  let uri = createCodeUri dropboxKey [("state", "dropbox.test-state-123")]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Dropbox
          , oauth2Key = dropboxKey
          , userApiUri = IDropbox.userInfoUri
          , toLoginUser = IDropbox.toLoginUser
          }
mkIDPData Facebook =
  let uri = createCodeUri facebookKey [ ("state", "facebook.test-state-123")
                                            , ("scope", "user_about_me,email")
                                            ]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Facebook
          , oauth2Key = facebookKey
          , userApiUri = IFacebook.userInfoUri
          , toLoginUser = IFacebook.toLoginUser
          }
mkIDPData Fitbit =
  let uri = createCodeUri fitbitKey [("state", "fitbit.test-state-123")
                                    , ("scope", "profile")
                                    ]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Fitbit
          , oauth2Key = fitbitKey
          , userApiUri = IFitbit.userInfoUri
          , toLoginUser = IFitbit.toLoginUser
          }

mkIDPData Github =
  let uri = createCodeUri githubKey [("state", "github.test-state-123")]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Github
          , oauth2Key = githubKey
          , userApiUri = IGithub.userInfoUri
          , toLoginUser = IGithub.toLoginUser
          }
mkIDPData Google =
  let uri = createCodeUri googleKey [ ("scope", "https://www.googleapis.com/auth/userinfo.email")
                                    , ("state", "google.test-state-123")
                                    ]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Google
          , oauth2Key = googleKey
          , userApiUri = IGoogle.userInfoUri
          , toLoginUser = IGoogle.toLoginUser
          }
mkIDPData StackExchange =
  let uri = createCodeUri stackexchangeKey [("state", "stackexchange.test-state-123")]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = StackExchange
          , oauth2Key = stackexchangeKey
          , userApiUri = IStackExchange.userInfoUri
          , toLoginUser = IStackExchange.toLoginUser
          }
mkIDPData Weibo =
  let uri = createCodeUri weiboKey [("state", "weibo.test-state-123")]
  in
  IDPData { codeFlowUri = uri
          , loginUser = Nothing
          , idpName = Weibo
          , oauth2Key = weiboKey
          , userApiUri = IWeibo.userInfoUri
          , toLoginUser = IWeibo.toLoginUser
          }

getUserInfo :: IDPData -> Manager -> AccessToken -> IO (Either Text LoginUser)
getUserInfo idpData mgr token = do
  case (idpName idpData) of
    Dropbox -> getDropboxUser idpData mgr token
    Weibo -> getWeiboUser idpData mgr token
    _ -> getUserInfoInteral idpData mgr token

getUserInfoInteral :: IDPData -> Manager -> AccessToken -> IO (Either Text LoginUser)
getUserInfoInteral (IDPData _ _ _ _ userApiUri toLoginUser) mgr token = do
  re <- authGetJSON mgr token userApiUri
  return (bimap showGetError toLoginUser re)

showGetError :: OAuth2Error Errors -> Text
showGetError = TL.pack . show

-- Dropbox API request
-- set token in header
-- nothing for body
-- content-type: application/json
getDropboxUser (IDPData _ _ _ _ userApiUri toLoginUser) mgr token = do
  re <- parseResponseJSON <$> authPostBS3 mgr token userApiUri []
  return (bimap showGetError toLoginUser re)

getWeiboUser (IDPData _ _ _ _ userApiUri toLoginUser) mgr token = do
  re <- parseResponseJSON <$> authGetBS' mgr token userApiUri
  return (bimap showGetError toLoginUser re)
