{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | [StackExchange authentication guide](https://api.stackexchange.com/docs/authentication)
--
--    * [StackExchange Apps page](https://stackapps.com/apps/oauth)
--
module Network.OAuth2.Provider.StackExchange where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString
import URI.ByteString.QQ

-- fix key from your application edit page
-- https://stackapps.com/apps/oauth
stackexchangeAppKey :: ByteString
stackexchangeAppKey = ""

data StackExchange = StackExchange deriving (Eq, Show)

type instance IdpUserInfo StackExchange = StackExchangeResp

defaultStackExchangeApp :: IdpApplication 'AuthorizationCode StackExchange
defaultStackExchangeApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.empty
    , idpAppAuthorizeState = "CHANGE_ME"
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppRedirectUri = [uri|http://localhost|]
    , idpAppName = "default-stackexchange-App"
    , idpAppTokenRequestAuthenticationMethod = ClientSecretPost
    , idp = defaultStackexchangeIdp
    }

defaultStackexchangeIdp :: Idp StackExchange
defaultStackexchangeIdp =
  Idp
    { idpFetchUserInfo = authGetJSONWithAuthMethod @_ @(IdpUserInfo StackExchange) AuthInRequestQuery
    , -- Only StackExchange has such specical app key which has to be append in userinfo uri.
      -- I feel it's not worth to invent a way to read from config
      -- file which would break the generic of Idp data type.
      -- Until discover a easier way, hard code for now.
      idpUserInfoEndpoint =
        appendStackExchangeAppKey
          [uri|https://api.stackexchange.com/2.2/me?site=stackoverflow|]
          stackexchangeAppKey
    , idpAuthorizeEndpoint = [uri|https://stackexchange.com/oauth|]
    , idpTokenEndpoint = [uri|https://stackexchange.com/oauth/access_token|]
    }

data StackExchangeResp = StackExchangeResp
  { hasMore :: Bool
  , quotaMax :: Integer
  , quotaRemaining :: Integer
  , items :: [StackExchangeUser]
  }
  deriving (Show, Generic)

data StackExchangeUser = StackExchangeUser
  { userId :: Integer
  , displayName :: Text
  , profileImage :: Text
  }
  deriving (Show, Generic)

instance FromJSON StackExchangeResp where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON StackExchangeUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

appendStackExchangeAppKey :: URI -> ByteString -> URI
appendStackExchangeAppKey useruri k = appendQueryParams [("key", k)] useruri
