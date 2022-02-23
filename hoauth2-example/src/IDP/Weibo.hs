{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module IDP.Weibo where

import Data.Aeson
import Data.Default
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString.QQ

newtype Weibo = Weibo IDP
  deriving (HasLabel, HasAuthUri, HasTokenRefreshReq, HasTokenReq)

weiboIdp :: IDP
weiboIdp =
  IDP
    { idpName = "weibo",
      oauth2Config = weiboKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.weibo.com/2/account/get_uid.json|]
    }

weiboKey :: OAuth2
weiboKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://api.weibo.com/oauth2/authorize|],
      oauth2TokenEndpoint = [uri|https://api.weibo.com/oauth2/access_token|]
    }

-- fetch user info via
-- GET
-- access token in query param only
instance HasUserReq Weibo where
  userReq (Weibo IDP {..}) mgr at = do
    re <- authGetJSONInternal [AuthInRequestQuery] mgr at oauth2UserInfoUri
    return (toLoginUser re)

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
