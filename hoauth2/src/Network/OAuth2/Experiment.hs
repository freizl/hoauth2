-- | This module contains a new way of doing OAuth2 authorization and authentication
-- in order to obtain Access Token and maybe Refresh Token base on rfc6749.
--
-- This module will become default in future release.
--
-- The key concept/change is to introduce the Grant flow, which determines the entire work flow per spec.
-- Each work flow will have slight different request parameters, which often time you'll see
-- different configuration when creating OAuth2 application in the IdP developer application page.
--
-- Here are supported flows
--
-- 1. Authorization Code. This flow requires authorize call to obtain an authorize code,
-- then exchange the code for tokens.
--
-- 2. Resource Owner Password. This flow only requires to hit token endpoint with, of course,
-- username and password, to obtain tokens.
--
-- 3. Client Credentials. This flow also only requires to hit token endpoint but with different parameters.
-- Client credentials flow does not involve an end user hence you won't be able to hit userinfo endpoint
-- with access token obtained.
--
-- 5. PKCE (rfc7636). This is enhancement on top of authorization code flow.
--
-- Implicit flow is not supported because it is more for SPA (single page app)
-- given it is deprecated by Authorization Code flow with PKCE.
--
-- Here is quick sample for how to use vocabularies from this new module.
--
-- Firstly, initialize your IdP (use google as example) and the application.
--
-- @
--
-- import Network.OAuth2.Experiment
-- import URI.ByteString.QQ
--
-- data Google = Google deriving (Eq, Show)
--
-- googleIdp :: Idp Google
-- googleIdp =
--   Idp
--     { idpAuthorizeEndpoint = [uri|https:\/\/accounts.google.com\/o\/oauth2\/v2\/auth|]
--     , idpTokenEndpoint = [uri|https:\/\/oauth2.googleapis.com\/token|]
--     , idpUserInfoEndpoint = [uri|https:\/\/www.googleapis.com\/oauth2\/v2\/userinfo|]
--     , idpDeviceAuthorizationEndpoint = Just [uri|https:\/\/oauth2.googleapis.com\/device\/code|]
--     }
--
-- fooApp :: AuthorizationCodeApplication
-- fooApp =
--   AuthorizationCodeApplication
--     { acClientId = "xxxxx",
--       acClientSecret = "xxxxx",
--       acScope =
--         Set.fromList
--           [ \"https:\/\/www.googleapis.com\/auth\/userinfo.email\",
--             \"https:\/\/www.googleapis.com\/auth\/userinfo.profile\"
--           ],
--       acAuthorizeState = \"CHANGE_ME\",
--       acAuthorizeRequestExtraParams = Map.empty,
--       acRedirectUri = [uri|http:\/\/localhost\/oauth2\/callback|],
--       acName = "sample-google-authorization-code-app",
--       acClientAuthenticationMethod = ClientSecretBasic,
--     }
--
-- fooIdpApplication :: IdpApplication AuthorizationCodeApplication Google
-- fooIdpApplication = IdpApplication fooApp googleIdp
-- @
--
-- Secondly, construct the authorize URL.
--
-- @
-- authorizeUrl = mkAuthorizationRequest fooIdpApplication
-- @
--
-- Thirdly, after a successful redirect with authorize code,
-- you could exchange for access token
--
-- @
-- mgr <- liftIO $ newManager tlsManagerSettings
-- tokenResp <- conduitTokenRequest fooIdpApplication mgr authorizeCode
-- @
--
-- If you'd like to fetch user info, uses this method
--
-- @
-- conduitUserInfoRequest fooIdpApplication mgr (accessToken tokenResp)
-- @
--
-- You could also find example from @hoauth2-providers-tutorials@ module.
module Network.OAuth2.Experiment (
  -- * Application per Grant type
  module Network.OAuth2.Experiment.Grants,

  -- * Authorization Code
  mkAuthorizationRequest,
  mkPkceAuthorizeRequest,

  -- * Device Authorization
  module Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest,
  conduitDeviceAuthorizationRequest,
  pollDeviceTokenRequest,

  -- * Token Request
  module Network.OAuth2.Experiment.Flows.TokenRequest,
  conduitPkceTokenRequest,
  conduitTokenRequest,

  -- * Refresh Token Request
  module Network.OAuth2.Experiment.Flows.RefreshTokenRequest,
  conduitRefreshTokenRequest,

  -- * UserInfo Request
  module Network.OAuth2.Experiment.Flows.UserInfoRequest,
  conduitUserInfoRequest,
  conduitUserInfoRequestWithCustomMethod,

  -- * Types
  module Network.OAuth2.Experiment.Types,
  module Network.OAuth2.Experiment.Pkce,
  module Network.OAuth2,

  -- * Utils
  module Network.OAuth2.Experiment.Utils,
) where

import Network.OAuth2 (ClientAuthenticationMethod (..))
import Network.OAuth2.Experiment.Flows
import Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest (
  DeviceAuthorizationResponse (..),
 )
import Network.OAuth2.Experiment.Flows.RefreshTokenRequest (
  HasRefreshTokenRequest,
 )
import Network.OAuth2.Experiment.Flows.TokenRequest (
  ExchangeTokenInfo,
  HasTokenRequest,
  NoNeedExchangeToken (..),
  TokenRequest,
 )
import Network.OAuth2.Experiment.Flows.UserInfoRequest (
  HasUserInfoRequest,
 )
import Network.OAuth2.Experiment.Grants
import Network.OAuth2.Experiment.Pkce (
  CodeVerifier (..),
 )
import Network.OAuth2.Experiment.Types (
  AuthorizeState (..),
  ClientId (..),
  ClientSecret (..),
  Idp (..),
  IdpApplication (..),
  Password (..),
  RedirectUri (..),
  Scope (..),
  Username (..),
 )
import Network.OAuth2.Experiment.Utils (uriToText)
