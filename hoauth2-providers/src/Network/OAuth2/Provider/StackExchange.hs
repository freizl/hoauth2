{-# LANGUAGE QuasiQuotes #-}

-- | [StackExchange authentication guide](https://api.stackexchange.com/docs/authentication)
--
--    * [StackExchange Apps page](https://stackapps.com/apps/oauth)
module Network.OAuth2.Provider.StackExchange where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString (URI)
import URI.ByteString.QQ (uri)

-- fix key from your application edit page
-- https://stackapps.com/apps/oauth
stackexchangeAppKey :: ByteString
stackexchangeAppKey = ""

userInfoEndpoint :: URI
userInfoEndpoint =
  appendQueryParams
    [ ("key", stackexchangeAppKey)
    , ("site", "stackoverflow")
    ]
    [uri|https://api.stackexchange.com/2.2/me|]

type instance IdpUserInfo StackExchange = StackExchangeResp

sampleStackExchangeAuthorizationCodeApp :: AuthorizationCodeApplication
sampleStackExchangeAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-stackexchange-authorization-code-app"
    , acTokenRequestAuthenticationMethod = ClientSecretPost
    }

fetchUserInfoMethod ::
  (FromJSON a, MonadIO m) =>
  Manager ->
  AccessToken ->
  URI ->
  ExceptT BSL.ByteString m a
fetchUserInfoMethod = authGetJSONWithAuthMethod AuthInRequestQuery

defaultStackExchangeIdp :: Idp StackExchange
defaultStackExchangeIdp =
  Idp
    { -- Only StackExchange has such specical app key which has to be append in userinfo uri.
      -- I feel it's not worth to invent a way to read from config
      -- file which would break the generic of Idp data type.
      -- Until discover a easier way, hard code for now.
      idpUserInfoEndpoint = userInfoEndpoint
    , idpAuthorizeEndpoint = [uri|https://stackexchange.com/oauth|]
    , idpTokenEndpoint = [uri|https://stackexchange.com/oauth/access_token|]
    , idpDeviceAuthorizationEndpoint = Nothing
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
