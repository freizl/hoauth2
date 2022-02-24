{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

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
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ

-- fix key from your application edit page
-- https://stackapps.com/apps/oauth
stackexchangeAppKey :: ByteString
stackexchangeAppKey = ""

data StackExchange = StackExchange deriving (Eq, Show)

type instance IDPUserInfo StackExchange = StackExchangeResp

type instance IDPName StackExchange = StackExchange

stackexchangeIdp :: IDP StackExchange
stackexchangeIdp =
  def
    { idpName = StackExchange,
      oauth2Config = stackexchangeKey,
      oauth2FetchAccessToken = fetchAccessTokenInternal ClientSecretPost,
      oauth2RefreshAccessToken = refreshAccessTokenInternal ClientSecretPost,
      convertUserInfoToLoginUser = toLoginUser,
      oauth2FetchUserInfo = fetchUserInfo,
      --
      -- Only StackExchange has such specical app key which has to be append in userinfo uri.
      -- I feel it's not worth to invent a way to read from config
      -- file which would break the generic of IDP data type.
      -- Until discover a easier way, hard code for now.
      --
      oauth2UserInfoUri =
        appendStackExchangeAppKey
          [uri|https://api.stackexchange.com/2.2/me?site=stackoverflow|]
          stackexchangeAppKey
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
  authGetJSONInternal
    [AuthInRequestQuery]
    mgr
    accessToken
    oauth2UserInfoUri

stackexchangeKey :: OAuth2
stackexchangeKey =
  def
    { oauth2AuthorizeEndpoint = [uri|https://stackexchange.com/oauth|],
      oauth2TokenEndpoint =
        [uri|https://stackexchange.com/oauth/access_token|]
    }

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
