{-# LANGUAGE QuasiQuotes #-}

-- | [DropBox oauth guide](https://developers.dropbox.com/oauth-guide)
module Network.OAuth2.Provider.DropBox where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.HTTP.Conduit (Manager)
import Network.OAuth2
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString.QQ

sampleDropBoxAuthorizationCodeApp :: AuthorizationCodeApplication
sampleDropBoxAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-dropbox-authorization-code-app"
    , acClientAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequestWithCustomMethod (\mgr at url -> authPostJSON mgr at url [])

defaultDropBoxIdp :: Idp DropBox
defaultDropBoxIdp =
  Idp
    { idpAuthorizeEndpoint = [uri|https://www.dropbox.com/1/oauth2/authorize|]
    , idpTokenEndpoint = [uri|https://api.dropboxapi.com/oauth2/token|]
    , -- https://www.dropbox.com/developers/documentation/http/documentation#users-get_current_account
      idpUserInfoEndpoint = [uri|https://api.dropboxapi.com/2/users/get_current_account|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

newtype DropBoxUserName = DropBoxUserName {displayName :: Text}
  deriving (Show, Generic)

data DropBoxUser = DropBoxUser
  { email :: Text
  , name :: DropBoxUserName
  }
  deriving (Show, Generic)

instance FromJSON DropBoxUserName where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON DropBoxUser
