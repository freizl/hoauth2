{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-
  NOTES: stackexchange API spec and its document just sucks!
-}
module IDP.StackExchange where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Lens.Micro
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ

-- fix key from your application edit page
-- https://stackapps.com/apps/oauth
type AppKey = ByteString

data StackExchange = StackExchange IDP AppKey

stackexchangeIdp :: IDP
stackexchangeIdp =
  IDP
    { idpName = "stackexchange",
      oauth2Config = stackexchangeKey,
      oauth2Scopes = [],
      oauth2UserInfoUri = [uri|https://api.stackexchange.com/2.2/me?site=stackoverflow|]
    }

stackexchangeKey :: OAuth2
stackexchangeKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://stackexchange.com/oauth|],
      oauth2TokenEndpoint =
        [uri|https://stackexchange.com/oauth/access_token|]
    }

instance HasLabel StackExchange where
  idpLabel = const "StackExchange"

instance HasAuthUri StackExchange where
  authUri (StackExchange idp _) = createAuthorizeUri idp

instance HasTokenReq StackExchange where
  tokenReq (StackExchange (IDP {..}) _) mgr = fetchAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasTokenRefreshReq StackExchange where
  tokenRefreshReq (StackExchange (IDP {..}) _) mgr = refreshAccessTokenInternal ClientSecretPost mgr oauth2Config

instance HasUserReq StackExchange where
  userReq (StackExchange (IDP {..}) appKey) mgr token = do
    re <-
      authGetJSONInternal
        [AuthInRequestQuery]
        mgr
        token
        (oauth2UserInfoUri `appendStackExchangeAppKey` appKey)
    (return . toLoginUser) re

data StackExchangeResp = StackExchangeResp
  { hasMore :: Bool,
    quotaMax :: Integer,
    quotaRemaining :: Integer,
    items :: [StackExchangeUser]
  }
  deriving (Show, Generic)

data StackExchangeUser = StackExchangeUser
  { userId :: Integer,
    displayName :: Text,
    profileImage :: Text
  }
  deriving (Show, Generic)

instance FromJSON StackExchangeResp where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON StackExchangeUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

toLoginUser :: StackExchangeResp -> LoginUser
toLoginUser StackExchangeResp {..} =
  case items of
    [] -> LoginUser {loginUserName = TL.pack "Cannot find stackexchange user"}
    (user : _) -> LoginUser {loginUserName = displayName user}

appendStackExchangeAppKey :: URI -> ByteString -> URI
appendStackExchangeAppKey useruri k =
  over (queryL . queryPairsL) (\query -> query ++ [("key", k)]) useruri
