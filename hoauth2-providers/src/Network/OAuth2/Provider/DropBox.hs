{-# LANGUAGE QuasiQuotes #-}

-- | [DropBox oauth guide](https://developers.dropbox.com/oauth-guide)
module Network.OAuth2.Provider.DropBox where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import URI.ByteString.QQ

type instance IdpUserInfo DropBox = DropBoxUser

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
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    }

defaultDropBoxIdp :: Idp DropBox
defaultDropBoxIdp =
  Idp
    { idpFetchUserInfo = \mgr at url -> authPostJSON @(IdpUserInfo DropBox) mgr at url []
    , idpAuthorizeEndpoint = [uri|https://www.dropbox.com/1/oauth2/authorize|]
    , idpTokenEndpoint = [uri|https://api.dropboxapi.com/oauth2/token|]
    , idpUserInfoEndpoint = [uri|https://api.dropboxapi.com/2/users/get_current_account|]
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
