{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module IDP.Weibo where

import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default
import qualified Data.Set as Set
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

data Weibo = Weibo deriving (Eq, Show)

type instance IDPUserInfo Weibo = WeiboUID

type instance IDPName Weibo = Weibo

weiboIdp :: IDP Weibo
weiboIdp =
  def
    { idpName = Weibo,
      oauth2Config = weiboKey,
      oauth2FetchUserInfo = fetchUserInfo,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2UserInfoUri = [uri|https://api.weibo.com/2/account/get_uid.json|]
    }

weiboKey :: OAuth2
weiboKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://api.weibo.com/oauth2/authorize|],
      oauth2TokenEndpoint = [uri|https://api.weibo.com/oauth2/access_token|]
    }

fetchUserInfo ::
  FromJSON b =>
  IDP a ->
  Manager ->
  AccessToken ->
  ExceptT
    BSL.ByteString
    IO
    b
fetchUserInfo IDP {..} mgr accessToken =
  authGetJSONWithAuthMethod
    (Set.fromList [AuthInRequestQuery])
    mgr
    accessToken
    oauth2UserInfoUri

-- | UserInfor API: http://open.weibo.com/wiki/2/users/show
data WeiboUser = WeiboUser
  { id :: Integer,
    name :: Text,
    screenName :: Text
  }
  deriving (Show, Generic)

newtype WeiboUID = WeiboUID {uid :: Integer}
  deriving (Show, Generic)

instance FromJSON WeiboUID where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON WeiboUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: WeiboUID -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = TL.pack $ show $ uid ouser}
