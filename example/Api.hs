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
import           Network.HTTP.Conduit
import           GHC.Generics

import Types
import qualified IDP.Okta as IOkta
import qualified IDP.Github as IGithub
import qualified IDP.Google as IGoogle

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

data UserApi = forall a . FromJSON a
  => UserApi { userApiUri :: URI
             , toLoginUser :: a -> LoginUser
             }

getUserApi :: IDP -> UserApi
getUserApi Okta = UserApi { userApiUri = IOkta.userInfoUri
                          , toLoginUser = IOkta.toLoginUser
                          }
getUserApi Github = UserApi { userApiUri = IGithub.userInfoUri
                            , toLoginUser = IGithub.toLoginUser
                            }
getUserApi Google = UserApi { userApiUri = IGoogle.userInfoUri
                            , toLoginUser = IGoogle.toLoginUser
                            }

getUserInfo :: IDP -> Manager -> AccessToken -> IO (Either Text LoginUser)
getUserInfo idp mgr token = do
  let userApi = getUserApi idp
  getUserInfoInteral userApi mgr token

getUserInfoInteral :: UserApi -> Manager -> AccessToken -> IO (Either Text LoginUser)
getUserInfoInteral (UserApi uri toUser) mgr token = do
  re <- authGetJSON mgr token uri
  return (bimap showGetError (toUser) re)

showGetError :: OAuth2Error Errors -> Text
showGetError = TL.pack . show

