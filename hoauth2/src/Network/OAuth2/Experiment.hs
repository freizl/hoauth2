{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains a new way of doing OAuth2 authorization and authentication
-- in order to obtain Access Token and maybe Refresh Token base on rfc6749.
--
-- This module will become default in future release. (TBD but likely 3.0).
--
-- The key concept/change is to introduce the 'GrantType', which determines the entire work flow per spec.
--
-- Each work flow will have slight different request parameters, which often time you'll see
-- different configuration when creating OAuth2 application in the IdP developer application page.
--
-- In a nutshell
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
-- 4. Implicit flow. This is more for SPA (single page app) and more or less obsolete by
-- Authorization Code flow with PKCE hence no intent to support it.
--
-- 5. PKCE (rfc7636). This is enhancement on top of authorization code flow.
--
-- Here is quick sample for how to use vocabulary from this new module.
--
-- Firstly, initialize your IdP (use google as example) and the application.
--
-- @
-- data Google = Google deriving (Eq, Show)
-- googleIdp = Idp Google
--   Idp
--     { idpFetchUserInfo = authGetJSON @(IdpUserInfo Google),
--       idpAuthorizeEndpoint = [uri|https://accounts.google.com/o/oauth2/v2/auth|],
--       idpTokenEndpoint = [uri|https://oauth2.googleapis.com/token|],
--       idpUserInfoEndpoint = [uri|https://www.googleapis.com/oauth2/v2/userinfo|]
--     }
--
-- fooApp :: IdpApplication 'AuthorizationCode Google
-- fooApp =
--   AuthorizationCodeIdpApplication
--     { idpAppClientId = "xxxxx",
--       idpAppClientSecret = "xxxxx",
--       idpAppScope =
--         Set.fromList
--           [ \"https://www.googleapis.com/auth/userinfo.email\",
--             \"https://www.googleapis.com/auth/userinfo.profile\"
--           ],
--       idpAppAuthorizeState = \"CHANGE_ME\",
--       idpAppAuthorizeExtraParams = Map.empty,
--       idpAppRedirectUri = [uri|http://localhost/oauth2/callback|],
--       idpAppName = "default-google-App",
--       idpAppTokenRequestAuthenticationMethod = ClientSecretBasic,
--       idp = googleIdp
--     }
-- @
--
-- Secondly, construct the authorize URL.
--
-- @
-- authorizeUrl = mkAuthorizeRequest fooApp
-- @
--
-- Thirdly, after a successful redirect with authorize code,
-- you could exchange for access token
--
-- @
-- mgr <- liftIO $ newManager tlsManagerSettings
-- tokenResp <- conduitTokenRequest fooApp mgr authorizeCode
-- @
--
-- Lastly, you probably like to fetch user info
--
-- @
-- conduitUserInfoRequest fooApp mgr (accessToken tokenResp)
-- @
--
-- Also you could find example from @hoauth2-providers-tutorials@ module.
--
module Network.OAuth2.Experiment
  ( module Network.OAuth2.Experiment.Types,
    module Network.OAuth2.Experiment.Pkce
  )
where

import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
