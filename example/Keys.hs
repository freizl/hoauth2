{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Keys where

import           Data.ByteString                ( ByteString )
import           Network.OAuth.OAuth2
import           URI.ByteString.QQ

weiboKey :: OAuth2
weiboKey = OAuth2
  { oauth2ClientId            = "xxxxxxxxxxxxxxx"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://127.0.0.1:9988/oauthCallback|]
  , oauth2AuthorizeEndpoint  = [uri|https://api.weibo.com/oauth2/authorize|]
  , oauth2TokenEndpoint = [uri|https://api.weibo.com/oauth2/access_token|]
  }

-- | http://developer.github.com/v3/oauth/
githubKey :: OAuth2
githubKey = OAuth2
  { oauth2ClientId            = "xxxxxxxxxxxxxxx"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://127.0.0.1:9988/githubCallback|]
  , oauth2AuthorizeEndpoint  = [uri|https://github.com/login/oauth/authorize|]
  , oauth2TokenEndpoint =
    [uri|https://github.com/login/oauth/access_token|]
  }

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
googleKey :: OAuth2
googleKey = OAuth2
  { oauth2ClientId            = "xxxxxxxxxxxxxxx.apps.googleusercontent.com"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://127.0.0.1:9988/googleCallback|]
  , oauth2AuthorizeEndpoint  = [uri|https://accounts.google.com/o/oauth2/auth|]
  , oauth2TokenEndpoint = [uri|https://www.googleapis.com/oauth2/v3/token|]
  }

facebookKey :: OAuth2
facebookKey = OAuth2
  { oauth2ClientId            = "xxxxxxxxxxxxxxx"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://t.haskellcn.org/cb|]
  , oauth2AuthorizeEndpoint  = [uri|https://www.facebook.com/dialog/oauth|]
  , oauth2TokenEndpoint =
    [uri|https://graph.facebook.com/v2.3/oauth/access_token|]
  }

doubanKey :: OAuth2
doubanKey = OAuth2
  { oauth2ClientId            = "xxxxxxxxxxxxxxx"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9999/oauthCallback|]
  , oauth2AuthorizeEndpoint  = [uri|https://www.douban.com/service/auth2/auth|]
  , oauth2TokenEndpoint = [uri|https://www.douban.com/service/auth2/token|]
  }

fitbitKey :: OAuth2
fitbitKey = OAuth2
  { oauth2ClientId            = "xxxxxx"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauth2AuthorizeEndpoint  = [uri|https://www.fitbit.com/oauth2/authorize|]
  , oauth2TokenEndpoint = [uri|https://api.fitbit.com/oauth2/token|]
  }

-- fix key from your application edit page
-- https://stackapps.com/apps/oauth
stackexchangeAppKey :: ByteString
stackexchangeAppKey = "xxxxxx"

stackexchangeKey :: OAuth2
stackexchangeKey = OAuth2
  { oauth2ClientId            = "xx"
  , oauth2ClientSecret        = Just "xxxxxxxxxxxxxxx"
  , oauth2RedirectUri            = Just [uri|http://c.haskellcn.org/cb|]
  , oauth2AuthorizeEndpoint  = [uri|https://stackexchange.com/oauth|]
  , oauth2TokenEndpoint =
    [uri|https://stackexchange.com/oauth/access_token|]
  }
dropboxKey :: OAuth2
dropboxKey = OAuth2
  { oauth2ClientId            = "xxx"
  , oauth2ClientSecret        = Just "xxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauth2AuthorizeEndpoint  = [uri|https://www.dropbox.com/1/oauth2/authorize|]
  , oauth2TokenEndpoint = [uri|https://api.dropboxapi.com/oauth2/token|]
  }

oktaKey :: OAuth2
oktaKey = OAuth2
  { oauth2ClientId            = "xxx"
  , oauth2ClientSecret        = Just "xxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauth2AuthorizeEndpoint  =
    [uri|https://hw2.trexcloud.com/oauth2/v1/authorize|]
  , oauth2TokenEndpoint =
    [uri|https://hw2.trexcloud.com/oauth2/v1/token|]
  }

azureADKey :: OAuth2
azureADKey = OAuth2
  { oauth2ClientId            = "xxx"
  , oauth2ClientSecret        = Just "xxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauth2AuthorizeEndpoint  =
    [uri|https://login.windows.net/common/oauth2/authorize|]
  , oauth2TokenEndpoint =
    [uri|https://login.windows.net/common/oauth2/token|]
  }

zohoKey :: OAuth2
zohoKey = OAuth2
  { oauth2ClientId            = "xxx"
  , oauth2ClientSecret        = Just "xxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauth2AuthorizeEndpoint  = [uri|https://accounts.zoho.com/oauth/v2/auth|]
  , oauth2TokenEndpoint = [uri|https://accounts.zoho.com/oauth/v2/token|]
  }

auth0Key :: OAuth2
auth0Key = OAuth2
  { oauth2ClientId            = "xxx"
  , oauth2ClientSecret        = Just "xxx"
  , oauth2RedirectUri            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauth2AuthorizeEndpoint  = [uri|https://freizl.auth0.com/authorize|]
  , oauth2TokenEndpoint = [uri|https://freizl.auth0.com/oauth/token|]
  }

-- https://api.slack.com/authentication/sign-in-with-slack
-- https://slack.com/.well-known/openid-configuration
slackKey :: OAuth2
slackKey =
  OAuth2
    { oauth2ClientId = ""
    , oauth2ClientSecret = Just ""
    , oauth2RedirectUri = Just [uri|http://localhost:9988/oauth2/callback|]
    , oauth2AuthorizeEndpoint = [uri|https://slack.com/openid/connect/authorize|]
    , oauth2TokenEndpoint = [uri|https://slack.com/api/openid.connect.token|]
    }
