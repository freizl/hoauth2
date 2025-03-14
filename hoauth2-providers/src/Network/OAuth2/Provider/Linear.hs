{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | [OAuth 2.0 Authentication](https://developers.linear.app/docs/oauth/authentication)
module Network.OAuth2.Provider.Linear where

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
import URI.ByteString.QQ

sampleLinearAuthorizationCodeApp :: AuthorizationCodeApplication
sampleLinearAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-linear-authorization-code-app"
    , acClientAuthenticationMethod = ClientSecretPost
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo =
  conduitUserInfoRequestWithCustomMethod
    (\mgr at url -> authPostJSON mgr at url [("query", "{ viewer { id name email } }")])

defaultLinearIdp :: Idp Linear
defaultLinearIdp =
  Idp
    { idpAuthorizeEndpoint = [uri|https://linear.app/oauth/authorize|]
    , idpTokenEndpoint = [uri|https://api.linear.app/oauth/token|]
    , idpUserInfoEndpoint = [uri|https://api.linear.app/graphql|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

-- sample response
-- {
--   "data": {
--     "viewer": {
--       "id": "***",
--       "name": "Hai W.",
--       "email": "freizl.em@gmail.com"
--     }
--   }
-- }

newtype LinearResponse a = LinearResponse
  { _data :: LinearData a
  }
  deriving (Show, Generic)

-- Define a data type for the "data" part of the JSON, which contains the viewer
newtype LinearData a = LinearData
  { _viewer :: a
  }
  deriving (Show, Generic)

data LinearUser = LinearUser
  { id :: Text
  , name :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON a => FromJSON (LinearData a) where
  parseJSON = withObject "Data" $ \v ->
    LinearData
      <$> v .: "viewer"

-- Define a custom FromJSON instance for Response to parse the outer structure
instance FromJSON a => FromJSON (LinearResponse a) where
  parseJSON = withObject "Response" $ \v ->
    LinearResponse
      <$> v .: "data"

instance FromJSON LinearUser where
  parseJSON = withObject "LinearUser" $ \v ->
    LinearUser
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "email"

getUser :: LinearResponse LinearUser -> LinearUser
getUser (LinearResponse (LinearData a)) = a
