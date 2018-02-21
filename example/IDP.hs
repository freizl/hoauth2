{-# LANGUAGE OverloadedStrings         #-}

module IDP where

import           Data.ByteString      (ByteString)
import qualified Data.Text.Encoding   as TE
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as TL
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
          , getAccessToken = IOkta.getAccessToken
          , getUserInfo = IOkta.getUserInfo
          }
mkIDPData Douban =
  let userUri = createCodeUri doubanKey [("state", "douban.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Douban
          , oauth2Key = doubanKey
          , getAccessToken = IDouban.getAccessToken
          , getUserInfo = IDouban.getUserInfo
          }
mkIDPData Dropbox =
  let userUri = createCodeUri dropboxKey [("state", "dropbox.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Dropbox
          , oauth2Key = dropboxKey
          , getAccessToken = IDropbox.getAccessToken
          , getUserInfo = IDropbox.getUserInfo
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
          , getAccessToken = IFacebook.getAccessToken
          , getUserInfo = IFacebook.getUserInfo
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
          , getAccessToken = IFitbit.getAccessToken
          , getUserInfo = IFitbit.getUserInfo
          }

mkIDPData Github =
  let userUri = createCodeUri githubKey [("state", "github.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Github
          , oauth2Key = githubKey
          , getAccessToken = IGithub.getAccessToken
          , getUserInfo = IGithub.getUserInfo
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
          , getAccessToken = IGoogle.getAccessToken
          , getUserInfo = IGoogle.getUserInfo
          }
mkIDPData StackExchange =
  let userUri = createCodeUri stackexchangeKey [("state", "stackexchange.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = StackExchange
          , oauth2Key = stackexchangeKey
          , getAccessToken = IStackExchange.getAccessToken
          , getUserInfo = IStackExchange.getUserInfo
          }
mkIDPData Weibo =
  let userUri = createCodeUri weiboKey [("state", "weibo.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Weibo
          , oauth2Key = weiboKey
          , getAccessToken = IWeibo.getAccessToken
          , getUserInfo = IWeibo.getUserInfo
          }


