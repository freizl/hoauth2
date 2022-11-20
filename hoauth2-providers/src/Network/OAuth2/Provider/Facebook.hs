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
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import URI.ByteString.QQ

data Facebook = Facebook deriving (Eq, Show)

type instance IdpUserInfo Facebook = FacebookUser

defaultFacebookApp :: IdpApplication 'AuthorizationCode Facebook
defaultFacebookApp =
  AuthorizationCodeIdpApplication
    { idpAppClientId = ""
    , idpAppClientSecret = ""
    , idpAppScope = Set.fromList ["user_about_me", "email"]
    , idpAppAuthorizeExtraParams = Map.empty
    , idpAppAuthorizeState = "CHANGE_ME"
    , idpAppRedirectUri = [uri|http://localhost|]
    , idpAppName = "default-facebook-App"
    , idpAppTokenRequestAuthenticationMethod = ClientSecretPost
    , idp = defaultFacebookIdp
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
