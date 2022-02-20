{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Weibo where

import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Hashable
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

newtype Weibo = Weibo OAuth2 deriving (Show, Generic, Eq)

instance Hashable Weibo

instance IDP Weibo

instance HasLabel Weibo where
  idpLabel = const "Weibo"

instance HasTokenRefreshReq Weibo where
  tokenRefreshReq (Weibo key) mgr = refreshAccessToken mgr key

instance HasTokenReq Weibo where
  tokenReq (Weibo key) mgr = fetchAccessToken mgr key

-- fetch user info via
-- GET
-- access token in query param only
instance HasUserReq Weibo where
  userReq _ mgr at = do
    re <- authGetBS [AuthInRequestQuery] mgr at userInfoUri
    case eitherDecode re of
      Right obj -> return (toLoginUser obj)
      Left e -> throwE (BSL.pack e)

instance HasAuthUri Weibo where
  authUri (Weibo key) =
    createCodeUri
      key
      [ ("state", "Weibo.test-state-123")
      ]

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

userInfoUri :: URI
userInfoUri = [uri|https://api.weibo.com/2/account/get_uid.json|]

toLoginUser :: WeiboUID -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = TL.pack $ show $ uid ouser}
