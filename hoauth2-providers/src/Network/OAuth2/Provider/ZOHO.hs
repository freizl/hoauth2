{-# LANGUAGE QuasiQuotes #-}

-- | [ZOHO oauth overview](https://www.zoho.com/crm/developer/docs/api/v2/oauth-overview.html)
module Network.OAuth2.Provider.ZOHO where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
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
import URI.ByteString.QQ

type instance IdpUserInfo ZOHO = ZOHOUserResp

sampleZohoAuthorizationCodeApp :: AuthorizationCodeApplication
sampleZohoAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["ZohoCRM.users.READ"]
    , acAuthorizeRequestExtraParams = Map.fromList [("access_type", "offline"), ("prompt", "consent")]
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost/oauth2/callback|]
    , acName = "sample-zoho-authorization-code-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfoMethod ::
  (FromJSON a, MonadIO m) =>
  Manager ->
  AccessToken ->
  URI ->
  ExceptT BSL.ByteString m a
fetchUserInfoMethod = authGetJSON

defaultZohoIdp :: Idp ZOHO
defaultZohoIdp =
  Idp
    { idpUserInfoEndpoint = [uri|https://www.zohoapis.com/crm/v2/users|]
    , idpAuthorizeEndpoint = [uri|https://accounts.zoho.com/oauth/v2/auth|]
    , idpTokenEndpoint = [uri|https://accounts.zoho.com/oauth/v2/token|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

-- `oauth/user/info` url does not work and find answer from
-- https://help.zoho.com/portal/community/topic/oauth2-api-better-document-oauth-user-info
data ZOHOUser = ZOHOUser
  { email :: Text
  , fullName :: Text
  }
  deriving (Show, Generic)

newtype ZOHOUserResp = ZOHOUserResp {users :: [ZOHOUser]}
  deriving (Show, Generic)

instance FromJSON ZOHOUserResp

instance FromJSON ZOHOUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
