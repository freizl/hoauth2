-- | This module contains a new way of doing OAuth2 authorization and authentication
-- in order to obtain an access token and maybe a refresh token based on RFC 6749.
--
-- This module will become the default in a future release.
--
-- The key concept is introducing grant flows, which determine the entire workflow per the spec.
-- Each workflow has slightly different request parameters, which is why you'll often see
-- different configuration options when creating an OAuth2 application in an IdP developer console.
--
-- Here are the supported flows:
--
-- 1. Authorization Code. This flow requires an authorize call to obtain an authorization code,
-- then exchange the code for tokens.
--
-- 2. Resource Owner Password. This flow only requires hitting the token endpoint with, of course,
-- username and password, to obtain tokens.
--
-- 3. Client Credentials. This flow also only requires hitting the token endpoint, but with different parameters.
-- The client credentials flow does not involve an end user, so you won't be able to hit the userinfo endpoint
-- with the access token you obtain.
--
-- 4. PKCE (RFC 7636). This is an enhancement on top of the authorization code flow.
--
-- The implicit flow is not supported because it is primarily for SPAs (single-page apps),
-- and it has been deprecated in favor of the authorization code flow with PKCE.
--
-- Here is a quick sample showing how to use the vocabulary from this module.
--
-- First, initialize your IdP (using Google as an example) and the application.
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
-- Second, construct the authorize URL.
--
-- @
-- authorizeUrl = mkAuthorizationRequest fooIdpApplication
-- @
--
-- Third, after a successful redirect with an authorization code,
-- you can exchange it for an access token.
--
-- @
-- mgr <- liftIO $ newManager tlsManagerSettings
-- tokenResp <- conduitTokenRequest fooIdpApplication mgr authorizeCode
-- @
--
-- If you'd like to fetch user info, use this method:
--
-- @
-- conduitUserInfoRequest fooIdpApplication mgr (accessToken tokenResp)
-- @
--
-- You can also find an example in the @hoauth2-providers-tutorial@ module.
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
