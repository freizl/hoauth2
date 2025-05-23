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
import Network.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString (URI)
import URI.ByteString.QQ (uri)

-- Found the key from your application edit page
-- https://stackapps.com/apps/oauth
userInfoEndpoint :: ByteString -> URI
userInfoEndpoint stackexchangeAppKey =
  appendQueryParams
    [ ("key", stackexchangeAppKey)
    , ("site", "stackoverflow")
    ]
    [uri|https://api.stackexchange.com/2.2/me|]

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
    , acClientAuthenticationMethod = ClientSecretPost
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequestWithCustomMethod (authGetJSONWithAuthMethod AuthInRequestQuery)

defaultStackExchangeIdp :: ByteString -> Idp StackExchange
defaultStackExchangeIdp stackexchangeAppKey =
  Idp
    { -- Only StackExchange has such specical app key which has to be append in userinfo uri.
      -- I feel it's not worth to invent a way to read from config
      -- file which would break the generic of Idp data type.
      -- Until discover a easier way, hard code for now.
      idpUserInfoEndpoint = userInfoEndpoint stackexchangeAppKey
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
