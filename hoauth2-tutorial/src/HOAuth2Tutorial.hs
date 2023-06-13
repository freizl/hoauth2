{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | If you're hurry, go check source code directly.
--
-- = Configure your OAuth2 provider
--
-- Pick which OAuth2 provider you'd to use, e.g. Google, Github, Auth0 etc.
-- Pretty much all standard OAuth2 provider has developer portal to guide developer to use oauth2 flow.
-- So read it through if you're unfamiliar OAuth2 before.
-- Often time, those documents will guide you how to create an Application which has credentials
-- (e.g. @client_id@ and @client_secret@ for a web application), which will be used to authenticate your
-- service (replying party) with server.
--
-- For some OIDC providers, you may even be able to find out those URLs from a well-known endpoint.
--
-- @
-- https:\/\/BASE_DOMAIN\/.well-known\/openid-configuration
-- @
--
-- In this tutorial, I choose Auth0, which is one of existing OAuth2/OIDC Providers in the market.
-- This is the API Docs <https://auth0.com/docs/api>
--
-- = Generate Authorization URL.
--
-- OAuth2 starts with [authorization](https://www.rfc-editor.org/rfc/rfc6749#section-4).
--
-- To generate an authorization URL, call method 'authorizationUrl', then call 'appendQueryParams' to
-- append additional query parameters, e.g. @state@, @scope@ etc.
--
-- That method will also automatically append following query parameter to the authorization url.
--
-- @
-- client_id = 'xxx'        -- client id of your Application credential you got previously
-- response_type = 'code'   -- must be for authorization request
-- redirect_uri = 'xxx'     -- where does the server (provider) send back the authorization code.
--                        -- You have to config this when creating Application at previous step.
-- @
--
-- The generated URL looks like
--
-- @
-- https://DOMAIN/path/to/authorize?client_id=xxx&response_type=code&redirect_uri=xxx&state=xxx&scope=xxx&..
-- @
--
-- /Notes/: As of today, @hoauth2@ only supports @Code Grant@.
--
-- = Redirect user to the Authorization URL
--
-- Now you need to have your user to navigate to that URL to kick off OAuth flow.
--
-- There are different ways to redirect user to the 'authorizeUrl'.
--
-- e.g.
--
--   1. Display as anchor link directly at UI so that user can click it.
--
--   2. Create your own login endpoint, e.g. @/login@, which then 302 to the 'authorizeUrl'.
--
-- In this tutorial, I choose the second option. For instance this is how 'indexH' is implemented.
--
-- >>> setHeader "Location" (uriToText authorizeUrl)
-- >>> status status302
--
-- = Obtain Access Token
--
-- When user navigates to 'authorizeUrl', user will be prompt for login against the OAuth provider.
--
-- After an successful login there, user will be redirect back to your Application's @redirect_uri@
-- with @code@ in the query parameter.
--
-- With this @code@, we could exchange for an Access Token.
--
-- Also you'd better to validate the @state@ is exactly what you pass in the 'authorizeUrl'.
-- OAuth2 provider expects to send the exact @state@ back in the redirect request.
--
-- To obtain an Access Token, you could call 'fetchAccessToken',
-- which essentially takes the authorization @code@, make request to OAuth2 provider's @/token@ endpoint
-- to get an Access Token, plus some other information (see details at 'OAuth2Token').
--
-- 'fetchAccessToken' returns @ExceptT (OAuth2Error Errors) m OAuth2Token@
-- However Scotty, which is web framework I used to build this tutorial,
-- requires error as Text hence the transform with 'oauth2ErrorToText'
--
-- Once we got the 'OAuth2Token' (which actually deserves an better name like @TokenResponse@),
-- we could get the actual 'accessToken' of out it, use which to make API requests to resource server (often time same as the authorization server)
--
-- "Network.OAuth.OAuth2.HttpClient" provides a few handy method to send such API request.
-- For instance,
--
-- @
-- authGetJSON   -- Makes GET request and decode response as JSON, with access token appended in Authorization http header.
-- authPostJSON  -- Similar but does POST request
-- @
--
-- In this tutorial, it makes request to 'auth0UserInfoUri' to fetch Auth0 user information
-- so application knows who did the authorize.
--
-- = The end
--
-- That's it! Congratulations make thus far!
--
-- If you're interested more of OAuth2, keep reading on <https://www.oauth.com/>,
-- which provides a nice guide regarding what is OAuth2 and various use cases.
module HOAuth2Tutorial where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Generics (Generic)
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.HTTP.Types (status302)
import Network.OAuth.OAuth2 (
  ExchangeToken (ExchangeToken),
  OAuth2 (..),
  OAuth2Token (accessToken),
  TokenRequestError,
  appendQueryParams,
  authGetJSON,
  authorizationUrl,
  fetchAccessToken,
 )
import URI.ByteString (URI, serializeURIRef')
import URI.ByteString.QQ (uri)
import Web.Scotty (ActionM, scotty)
import Web.Scotty qualified as Scotty

------------------------------

-- * Configuration

------------------------------

auth0 :: OAuth2
auth0 =
  OAuth2
    { oauth2ClientId = "TZlmNRtLY9duT8M4ztgFBLsFA66aEoGs"
    , oauth2ClientSecret = ""
    , oauth2AuthorizeEndpoint = [uri|https://freizl.auth0.com/authorize|]
    , oauth2TokenEndpoint = [uri|https://freizl.auth0.com/oauth/token|]
    , oauth2RedirectUri = [uri|http://localhost:9988/oauth2/callback|]
    }

authorizeUrl :: URI
authorizeUrl =
  appendQueryParams
    [ ("scope", "openid profile email")
    , ("state", randomStateValue)
    ]
    $ authorizationUrl auth0

-- | You'll need to find out an better way to create @state@
-- which is recommended in <https://www.rfc-editor.org/rfc/rfc6749#section-10.12>
randomStateValue :: BS.ByteString
randomStateValue = "random-state-to-prevent-csrf"

-- | Endpoint for fetching user profile using access token
auth0UserInfoUri :: URI
auth0UserInfoUri = [uri|https://freizl.auth0.com/userinfo|]

-- | Auth0 user
-- https://auth0.com/docs/api/authentication#get-user-info
data Auth0User = Auth0User
  { name :: TL.Text
  , email :: TL.Text
  , sub :: TL.Text
  }
  deriving (Show, Generic)

instance FromJSON Auth0User

------------------------------

-- * Web server

------------------------------

-- | The 'scotty' application
app :: IO ()
app = do
  -- Poor man's solution for creating user session.
  refUser <- newIORef Nothing
  scotty 9988 $ do
    Scotty.get "/" $ indexH refUser
    Scotty.get "/login" loginH
    Scotty.get "/logout" (logoutH refUser)
    Scotty.get "/oauth2/callback" $ callbackH refUser

-- | @/@ endpoint handler
indexH :: IORef (Maybe Auth0User) -> ActionM ()
indexH refUser = do
  muser <- liftIO (readIORef refUser)

  let info = case muser of
        Just user ->
          [ "<p>Hello, " `TL.append` name user `TL.append` "</p>"
          , "<a href='/logout'>Logout</a>"
          ]
        Nothing -> ["<a href='/login'>Login</a>"]

  Scotty.html . mconcat $ "<h1>hoauth2 Tutorial</h1>" : info

-- | @/login@ endpoint handler
loginH :: ActionM ()
loginH = do
  Scotty.setHeader "Location" (uriToText authorizeUrl)
  Scotty.status status302

-- | @/logout@ endpoint handler
logoutH :: IORef (Maybe Auth0User) -> ActionM ()
logoutH refUser = do
  liftIO (writeIORef refUser Nothing)
  Scotty.redirect "/"

-- | @/oauth2/callback@ endpoint handler
callbackH :: IORef (Maybe Auth0User) -> ActionM ()
callbackH refUser = do
  pas <- Scotty.params

  excepttToActionM $ do
    void $ ExceptT $ pure $ paramValue "state" pas
    codeP <- ExceptT $ pure $ paramValue "code" pas

    mgr <- liftIO $ newManager tlsManagerSettings

    -- Exchange authorization code for Access Token
    -- 'oauth2ErrorToText' turns (OAuth2 error) to Text which is the default way
    -- Scotty represents error message
    let code = ExchangeToken $ TL.toStrict codeP
    tokenResp <- withExceptT oauth2ErrorToText (fetchAccessToken mgr auth0 code)

    -- Call API to resource server with Access Token being authentication code.
    -- 'bslToText' exists for similar reason as 'oauth2ErrorToText'
    let at = accessToken tokenResp
    user <- withExceptT bslToText (authGetJSON mgr at auth0UserInfoUri)

    -- Now we need to find way to set authentication status for this application
    -- that indicates user has been authenticated successfully.
    -- For simplicity in this tutorial, I choose an 'IORef'.
    liftIO $ writeIORef refUser (Just user)

  -- Where to navigate to after login page successfully.
  Scotty.redirect "/"

------------------------------

-- * Utilities

------------------------------

uriToText :: URI -> TL.Text
uriToText = TL.fromStrict . T.decodeUtf8 . serializeURIRef'

bslToText :: BSL.ByteString -> TL.Text
bslToText = TL.pack . BSL.unpack

paramValue ::
  -- | Parameter key
  TL.Text ->
  -- | All parameters
  [Scotty.Param] ->
  Either TL.Text TL.Text
paramValue key params =
  if null val
    then Left ("No value found for param: " <> key)
    else Right (head val)
  where
    val = snd <$> filter (hasParam key) params
    hasParam :: TL.Text -> Scotty.Param -> Bool
    hasParam t = (== t) . fst

-- | Lift ExceptT to ActionM which is basically the handler Monad in Scotty.
excepttToActionM :: (Show a) => ExceptT TL.Text IO a -> ActionM a
excepttToActionM e = do
  result <- liftIO $ runExceptT e
  either Scotty.raise pure result

oauth2ErrorToText :: TokenRequestError -> TL.Text
oauth2ErrorToText e = TL.pack $ "Unable fetch access token. error detail: " ++ show e
