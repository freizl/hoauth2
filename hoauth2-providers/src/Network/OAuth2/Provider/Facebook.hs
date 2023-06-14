{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | [Facebook Login](http://developers.facebook.com/docs/facebook-login/)
module Network.OAuth2.Provider.Facebook where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth2.Experiment
import Network.OAuth2.Experiment.GrantType.AuthorizationCode qualified as AuthorizationCode
import URI.ByteString.QQ

data Facebook = Facebook deriving (Eq, Show)

type instance IdpUserInfo Facebook = FacebookUser

defaultFacebookApp :: AuthorizationCode.Application
defaultFacebookApp =
  AuthorizationCode.Application
    { acClientId = ""
    , acClientSecret = ""
    , acScope = Set.fromList ["user_about_me", "email"]
    , acAuthorizeRequestExtraParams = Map.empty
    , acAuthorizeState = "CHANGE_ME"
    , acRedirectUri = [uri|http://localhost|]
    , acName = "default-facebook-App"
    , acTokenRequestAuthenticationMethod = ClientSecretPost
    }

defaultFacebookIdp :: Idp Facebook
defaultFacebookIdp =
  Idp
    { idpFetchUserInfo = authGetJSON @(IdpUserInfo Facebook)
    , idpUserInfoEndpoint = [uri|https://graph.facebook.com/me?fields=id,name,email|]
    , idpAuthorizeEndpoint = [uri|https://www.facebook.com/dialog/oauth|]
    , idpTokenEndpoint = [uri|https://graph.facebook.com/v2.3/oauth/access_token|]
    }

data FacebookUser = FacebookUser
  { id :: Text
  , name :: Text
  , email :: Text
  }
  deriving (Show, Generic)

instance FromJSON FacebookUser
