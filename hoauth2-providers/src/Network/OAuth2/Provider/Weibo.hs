{-# LANGUAGE QuasiQuotes #-}

-- | [微博授权机制](https://open.weibo.com/wiki/%E6%8E%88%E6%9D%83%E6%9C%BA%E5%88%B6)
module Network.OAuth2.Provider.Weibo where

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

sampleWeiboAuthorizationCodeApp :: AuthorizationCodeApplication
sampleWeiboAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acName = "sample-weibo-authorization-code-app"
    , acClientId = ""
    , acClientSecret = ""
    , acScope = Set.empty
    , acAuthorizeState = "CHANGE_ME"
    , acAuthorizeRequestExtraParams = Map.empty
    , acRedirectUri = [uri|http://localhost|]
    , acClientAuthenticationMethod = ClientSecretBasic
    }

fetchUserInfo ::
  (MonadIO m, HasUserInfoRequest a, FromJSON b) =>
  IdpApplication i a ->
  Manager ->
  AccessToken ->
  ExceptT BSL.ByteString m b
fetchUserInfo = conduitUserInfoRequestWithCustomMethod (authGetJSONWithAuthMethod AuthInRequestQuery)

defaultWeiboIdp :: Idp Weibo
defaultWeiboIdp =
  Idp
    { idpUserInfoEndpoint = [uri|https://api.weibo.com/2/account/get_uid.json|]
    , idpAuthorizeEndpoint = [uri|https://api.weibo.com/oauth2/authorize|]
    , idpTokenEndpoint = [uri|https://api.weibo.com/oauth2/access_token|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

-- | http://open.weibo.com/wiki/2/users/show
data WeiboUser = WeiboUser
  { id :: Integer
  , name :: Text
  , screenName :: Text
  }
  deriving (Show, Generic)

newtype WeiboUID = WeiboUID {uid :: Integer}
  deriving (Show, Generic)

instance FromJSON WeiboUID

instance FromJSON WeiboUser where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}
