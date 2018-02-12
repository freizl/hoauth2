{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Api where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.ByteString      (ByteString)
import qualified Data.Text.Encoding   as TE
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import           URI.ByteString

import qualified IDP.Douban           as IDouban
import qualified IDP.Dropbox          as IDropbox
import qualified IDP.Facebook         as IFacebook
import qualified IDP.Fitbit           as IFitbit
import qualified IDP.Github           as IGithub
import qualified IDP.Google           as IGoogle
import qualified IDP.Okta             as IOkta
import qualified IDP.StackExchange    as IStackExchange
import qualified IDP.Weibo            as IWeibo
import           Keys
import           Types

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

createCodeUri :: OAuth2
              -> [(ByteString, ByteString)]
              -> Text
createCodeUri key params = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams params
  $ authorizationUrl key

mkIDPData :: IDP -> IDPData
mkIDPData Okta =
  let userUri = createCodeUri oktaKey [("scope", "openid profile"), ("state", "okta.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Okta
          , oauth2Key = oktaKey
          , userApiUri = IOkta.userInfoUri
          , toLoginUser = IOkta.toLoginUser
          }
mkIDPData Douban =
  let userUri = createCodeUri doubanKey [("state", "douban.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Douban
          , oauth2Key = doubanKey
          , userApiUri = IDouban.userInfoUri
          , toLoginUser = IDouban.toLoginUser
          }
mkIDPData Dropbox =
  let userUri = createCodeUri dropboxKey [("state", "dropbox.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Dropbox
          , oauth2Key = dropboxKey
          , userApiUri = IDropbox.userInfoUri
          , toLoginUser = IDropbox.toLoginUser
          }
mkIDPData Facebook =
  let userUri = createCodeUri facebookKey [ ("state", "facebook.test-state-123")
                                            , ("scope", "user_about_me,email")
                                            ]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Facebook
          , oauth2Key = facebookKey
          , userApiUri = IFacebook.userInfoUri
          , toLoginUser = IFacebook.toLoginUser
          }
mkIDPData Fitbit =
  let userUri = createCodeUri fitbitKey [("state", "fitbit.test-state-123")
                                    , ("scope", "profile")
                                    ]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Fitbit
          , oauth2Key = fitbitKey
          , userApiUri = IFitbit.userInfoUri
          , toLoginUser = IFitbit.toLoginUser
          }

mkIDPData Github =
  let userUri = createCodeUri githubKey [("state", "github.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Github
          , oauth2Key = githubKey
          , userApiUri = IGithub.userInfoUri
          , toLoginUser = IGithub.toLoginUser
          }
mkIDPData Google =
  let userUri = createCodeUri googleKey [ ("scope", "https://www.googleapis.com/auth/userinfo.email")
                                    , ("state", "google.test-state-123")
                                    ]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Google
          , oauth2Key = googleKey
          , userApiUri = IGoogle.userInfoUri
          , toLoginUser = IGoogle.toLoginUser
          }
mkIDPData StackExchange =
  let userUri = createCodeUri stackexchangeKey [("state", "stackexchange.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = StackExchange
          , oauth2Key = stackexchangeKey
          , userApiUri = IStackExchange.userInfoUri
          , toLoginUser = IStackExchange.toLoginUser
          }
mkIDPData Weibo =
  let userUri = createCodeUri weiboKey [("state", "weibo.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Weibo
          , oauth2Key = weiboKey
          , userApiUri = IWeibo.userInfoUri
          , toLoginUser = IWeibo.toLoginUser
          }

getUserInfo :: IDPData -> Manager -> AccessToken -> IO (Either Text LoginUser)
getUserInfo idpD mgr token =
  case idpName idpD of
    Dropbox       -> getDropboxUser idpD mgr token
    Weibo         -> getWeiboUser idpD mgr token
    StackExchange -> getStackExchangeUser idpD mgr token
    _             -> getUserInfoInteral idpD mgr token

getUserInfoInteral :: IDPData -> Manager -> AccessToken -> IO (Either Text LoginUser)
getUserInfoInteral (IDPData _ _ _ _ userUri toUser) mgr token = do
  re <- authGetJSON mgr token userUri
  return (bimap showGetError toUser re)

showGetError :: OAuth2Error Errors -> Text
showGetError = TL.pack . show

getDropboxUser, getWeiboUser, getStackExchangeUser :: IDPData
  -> Manager
  -> AccessToken
  -> IO (Either Text LoginUser)
-- Dropbox API request
-- set token in header
-- nothing for body
-- content-type: application/json
getDropboxUser (IDPData _ _ _ _ userUri toUser) mgr token = do
  re <- parseResponseJSON <$> authPostBS3 mgr token userUri []
  return (bimap showGetError toUser re)

getWeiboUser (IDPData _ _ _ _ userUri toUser) mgr token = do
  re <- parseResponseJSON <$> authGetBS' mgr token userUri
  return (bimap showGetError toUser re)

getStackExchangeUser (IDPData _ _ _ _ userUri toUser) mgr token = do
  re <- parseResponseJSON <$> authGetBS' mgr token userUri
  return (bimap showGetError toUser re)

