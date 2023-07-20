{-# LANGUAGE QuasiQuotes #-}

-- | [Twitter OAuth2 guide](https://developer.twitter.com/en/docs/authentication/oauth-2-0/authorization-code)
module Network.OAuth2.Provider.Twitter where

import Data.Aeson
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import URI.ByteString.QQ

type instance IdpUserInfo Twitter = TwitterUserResp

sampleTwitterAuthorizationCodeApp :: AuthorizationCodeApplication
sampleTwitterAuthorizationCodeApp =
  AuthorizationCodeApplication
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["tweet.read", "users.read"]
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost|]
    , acName = "sample-twitter-authorization-code-app"
    , acTokenRequestAuthenticationMethod = ClientSecretBasic
    , acAuthorizeRequestExtraParams = Map.empty
    }

defaultTwitterIdp :: Idp Twitter
defaultTwitterIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Twitter)
    , idpUserInfoEndpoint = [uri|https://api.twitter.com/2/users/me|]
    , idpAuthorizeEndpoint = [uri|https://twitter.com/i/oauth2/authorize|]
    , idpTokenEndpoint = [uri|https://api.twitter.com/2/oauth2/token|]
    , idpDeviceAuthorizationEndpoint = Nothing
    }

data TwitterUser = TwitterUser
  { name :: Text
  , id :: Text
  , username :: Text
  }
  deriving (Show, Generic)

newtype TwitterUserResp = TwitterUserResp {twitterUserRespData :: TwitterUser}
  deriving (Show, Generic)

instance FromJSON TwitterUserResp where
  -- 15 = length "twitterUserResp"
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = map toLower . drop 15})

instance FromJSON TwitterUser
