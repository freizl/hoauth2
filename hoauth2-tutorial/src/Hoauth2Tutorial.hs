{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | hOAuth2 tutorial
--
-- TODO: consider literal haskell

module Hoauth2Tutorial where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef
import Data.Maybe
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest qualified as TR
import URI.ByteString
import URI.ByteString.QQ
import Web.Scotty

------------------------------

-- * Define your Provider

-- TODO: add comments
------------------------------

-- | Define which OAuth2 provider you'd to use, e.g. Google, Github, Auth0 etc
-- Pretty much all standard OAuth2 provider has developer portal
-- to guide developer to use oauth2 flow.
--
-- Most of time, Authorize and Token endpoint are pretty static.
-- For some OIDC providers, you may even be able to find out those URLs
-- from a well-known endpoint.
--
-- @
-- https:\/\/BASE_DOMAIN\/.well-known\/openid-configuration
-- @
--
-- In this tutorial, I choose Auth0, which is one of existings OAuth2/OIDC Providers in the market.
-- This is the API Docs <https://auth0.com/docs/api>
auth0 :: OAuth2
auth0 =
  OAuth2
    { oauth2ClientId = "TZlmNRtLY9duT8M4ztgFBLsFA66aEoGs",
      oauth2ClientSecret = "upvZlJuktI_M3yqRyJAo6_SU3Y8Lyu61CMOvzeFygdQtr6HmUIPQIbXx3r7iQcvS",
      oauth2AuthorizeEndpoint = [uri|https://freizl.auth0.com/authorize|],
      oauth2TokenEndpoint = [uri|https://freizl.auth0.com/oauth/token|],
      oauth2RedirectUri = [uri|http://localhost:9988/oauth2/callback|]
    }

-- * Kick off Authorization

-- | The first step of OAuth2 code flow is to figure out what is the authorize flow.
-- @hoauth2@ provides a handy method to create it.
-- If you need adding more parameter to the URL,
-- use 'authorizationUrlWithParams' with providing additional parameters
-- or 'appendQueryParams' by directly modified generated URL
authorizeUrl :: URI
authorizeUrl =
  authorizationUrlWithParams
    [ ("scope", "openid profile email"),
      ("state", randomStateValue)
    ]
    auth0

randomStateValue :: BS.ByteString
randomStateValue = "random-state-to-prevent-csrf"

-- | There are different way to redirect to the 'authorizeUrl' to kick off auth flow
--
-- e.g.
-- 1. I could display as anchor link directly at UI
-- 2. You could have your own login endpoint, e.g. @/login@, which then 302 to the 'authorizeUrl'.
--
-- In this tutorial, I choose the second option.
loginH :: ActionM ()
loginH = do
  setHeader "Location" (uriToText authorizeUrl)
  status status302

-- * Obtain Access Token

-- | In your callback handler, you shall be able to obtain @code@ from query parameter.
-- Also you'd better to validate the @state@ is exactly what you pass in the 'authorizeUrl'.
-- OAuth2 provider expects to send you back in the callback.
--
-- TODO: explain why state and nonce.
callbackH :: IORef (Maybe Auth0User) -> ActionM ()
callbackH refUser = do
  pas <- params

  let stateP = paramValue "state" pas
  when (null stateP) (raise "callbackH: no state from callback request")
  let codeP = paramValue "code" pas
  when (null codeP) (raise "callbackH: no code from callback request")

  excepttToActionM $ do
    mgr <- liftIO $ newManager tlsManagerSettings

    -- make request to /token endpoint to exchange 'code' for 'access token'
    let code = ExchangeToken $ TL.toStrict $ head codeP
    tokenResp <- withExceptT oauth2ErrorToText (fetchAccessToken mgr auth0 code)

    let at = accessToken tokenResp
    user <- withExceptT bslToText (authGetJSON mgr at auth0UserInfoUri)

    -- Now we need to find way to set authentication status for this application
    -- that user has been authenticated successfully.
    -- For simplicity in this tutorial, I choose an 'IORef'.
    liftIO $ writeIORef refUser (Just user)

  -- Navigate to the "after login page".
  redirect "/"

-- | Endpoint for fetching user profile using access token
auth0UserInfoUri :: URI
auth0UserInfoUri = [uri|https://freizl.auth0.com/userinfo|]

data Auth0User = Auth0User
  { name :: TL.Text,
    email :: TL.Text,
    sub :: TL.Text
  }
  deriving (Show, Generic)

instance FromJSON Auth0User where
  parseJSON =
    genericParseJSON defaultOptions

------------------------------

-- * Hook with your web server

-- TODO: add comments
------------------------------

myServerPort :: Int
myServerPort = 9988

app :: IO ()
app = do
  refUser <- newIORef Nothing
  scotty 9988 $ do
    get "/" $ indexH refUser
    get "/login" loginH
    get "/logout" (logoutH refUser)
    get "/oauth2/callback" $ callbackH refUser

indexH :: IORef (Maybe Auth0User) -> ActionM ()
indexH refUser = do
  muser <- liftIO (readIORef refUser)

  let info =
        if isJust muser
          then
            [ "<p>Hello, " `TL.append` name (fromJust muser) `TL.append` "</p>",
              "<a href='/logout'>Logout</a>"
            ]
          else ["<a href='/login'>Login</a>"]

  html . mconcat $ "<h1>hoauth2 Tutorial</h1>" ++ info

logoutH :: IORef (Maybe Auth0User) -> ActionM ()
logoutH refUser = do
  liftIO (writeIORef refUser Nothing)
  redirect "/"

------------------------------

-- * Utities

------------------------------

uriToText :: URI -> TL.Text
uriToText = TL.fromStrict . T.decodeUtf8 . serializeURIRef'

bslToText :: BSL.ByteString -> TL.Text
bslToText = TL.pack . BSL.unpack

paramValue :: TL.Text -> [Param] -> [TL.Text]
paramValue key = fmap snd . filter (hasParam key)

hasParam :: TL.Text -> Param -> Bool
hasParam t = (== t) . fst

excepttToActionM :: Show a => ExceptT TL.Text IO a -> ActionM a
excepttToActionM e = do
  result <- liftIO $ runExceptT e
  either raise return result

-- | align with Scotty ActionM monad
-- TODO: elaborate
oauth2ErrorToText :: OAuth2Error TR.Errors -> TL.Text
oauth2ErrorToText e = TL.pack $ "Unable fetch access token. error detail: " ++ show e
