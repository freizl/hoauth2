{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module HOAuth2ProvidersTutorial where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.HTTP.Types (status302)
import Network.OAuth.OAuth2 (
  ExchangeToken (ExchangeToken),
  OAuth2Token (accessToken),
  TokenRequestError,
 )
import Network.OAuth2.Experiment
import Network.OAuth2.Provider
import Network.OAuth2.Provider.Auth0 (Auth0User (..), mkAuth0Idp)
import Network.OAuth2.Provider.Google (GoogleUser (..))
import Network.OAuth2.Provider.Google qualified as Google
import URI.ByteString.QQ (uri)
import Web.Scotty (ActionM, scotty)
import Web.Scotty qualified as Scotty
import Prelude hiding (id)

------------------------------

-- * Configuration

------------------------------

mkTestAuth0App :: ExceptT Text IO (IdpApplication Auth0 AuthorizationCodeApplication)
mkTestAuth0App = do
  idp <- mkTestAuth0Idp
  let application =
        AuthorizationCodeApplication
          { acClientId = ""
          , acClientSecret = ""
          , acAuthorizeState = AuthorizeState ("auth0." <> randomStateValue)
          , acScope = Set.fromList ["openid", "email", "profile"]
          , acRedirectUri = [uri|http://localhost:9988/oauth2/callback|]
          , acName = "foo-auth0-app"
          , acAuthorizeRequestExtraParams = Map.empty
          , acTokenRequestAuthenticationMethod = ClientSecretBasic
          }
  pure IdpApplication {..}

mkTestAuth0Idp :: ExceptT Text IO (Idp Auth0)
mkTestAuth0Idp = mkAuth0Idp "freizl.auth0.com"

mkTestGoogleApp :: IdpApplication Google AuthorizationCodeApplication
mkTestGoogleApp =
  let application =
        AuthorizationCodeApplication
          { acClientId = ""
          , acClientSecret = ""
          , acAuthorizeState = AuthorizeState ("google." <> randomStateValue)
          , acRedirectUri = [uri|http://localhost:9988/oauth2/callback|]
          , acScope =
              Set.fromList
                [ "https://www.googleapis.com/auth/userinfo.email"
                , "https://www.googleapis.com/auth/userinfo.profile"
                ]
          , acName = "foo-google-app"
          , acAuthorizeRequestExtraParams = Map.empty
          , acTokenRequestAuthenticationMethod = ClientSecretBasic
          }
      idp = Google.defaultGoogleIdp
   in IdpApplication {..}

-- | You'll need to find out an better way to create @state@
-- which is recommended in <https://www.rfc-editor.org/rfc/rfc6749#section-10.12>
randomStateValue :: TL.Text
randomStateValue = "random-state-to-prevent-csrf"

------------------------------

-- * Web server

------------------------------
data DemoUser = DemoUser
  { name :: TL.Text
  , email :: Maybe TL.Text
  }
  deriving (Eq, Show)

-- | The 'scotty' application
app :: IO ()
app = do
  eAuth0App <- runExceptT mkTestAuth0App
  either (error . TL.unpack) runApp eAuth0App
  where
    runApp :: IdpApplication Auth0 AuthorizationCodeApplication -> IO ()
    runApp auth0App = do
      -- Poor man's solution for creating user session.
      refUser <- newIORef Nothing
      let googleApp = mkTestGoogleApp
      scotty 9988 $ do
        Scotty.get "/" $ indexH refUser
        Scotty.get "/login/auth0" (loginAuth0H auth0App)
        Scotty.get "/login/google" (loginGoogleH googleApp)
        Scotty.get "/logout" (logoutH refUser)
        Scotty.get "/oauth2/callback" $ callbackH auth0App googleApp refUser

-- | @/@ endpoint handler
indexH :: IORef (Maybe DemoUser) -> ActionM ()
indexH refUser = do
  muser <- liftIO (readIORef refUser)

  let info = case muser of
        Just DemoUser {..} ->
          [ "<h2>Hello, "
          , name
          , "</h2>"
          , "<p>"
          , TL.pack (show email)
          , "</p>"
          , "<a href='/logout'>Logout</a>"
          ]
        Nothing ->
          [ "<ul>"
          , "<li>"
          , "<a href='/login/auth0'>Login with Auth0</a>"
          , "</li>"
          , "<li>"
          , "<a href='/login/google'>Login with Google</a>"
          , "</li>"
          , "</ul>"
          ]

  Scotty.html . mconcat $ "<h1>hoauth2 providers Tutorial</h1>" : info

-- | @/login/auth0@ endpoint handler
loginAuth0H :: IdpApplication Auth0 AuthorizationCodeApplication -> ActionM ()
loginAuth0H auth0App = do
  Scotty.setHeader "Location" (TL.fromStrict $ uriToText $ mkAuthorizationRequest auth0App)
  Scotty.status status302

-- | @/login/google@ endpoint handler
loginGoogleH :: IdpApplication Google AuthorizationCodeApplication -> ActionM ()
loginGoogleH googleApp = do
  Scotty.setHeader "Location" (TL.fromStrict $ uriToText $ mkAuthorizationRequest googleApp)
  Scotty.status status302

-- | @/logout@ endpoint handler
logoutH :: IORef (Maybe DemoUser) -> ActionM ()
logoutH refUser = do
  liftIO (writeIORef refUser Nothing)
  Scotty.redirect "/"

-- | @/oauth2/callback@ endpoint handler
callbackH ::
  IdpApplication Auth0 AuthorizationCodeApplication ->
  IdpApplication Google AuthorizationCodeApplication ->
  IORef (Maybe DemoUser) ->
  ActionM ()
callbackH auth0App googleApp refUser = do
  pas <- Scotty.params

  excepttToActionM $ do
    state <- ExceptT $ pure $ paramValue "state" pas
    codeP <- ExceptT $ pure $ paramValue "code" pas

    let code = ExchangeToken $ TL.toStrict codeP
    let idpName = TL.takeWhile ('.' /=) state

    user <- case idpName of
      "google" -> handleGoogleCallback googleApp code
      "auth0" -> handleAuth0Callback auth0App code
      _ -> throwE $ "unable to find idp app of: " <> idpName

    liftIO $ writeIORef refUser (Just user)

  Scotty.redirect "/"

handleAuth0Callback ::
  IdpApplication Auth0 AuthorizationCodeApplication ->
  ExchangeToken ->
  ExceptT TL.Text IO DemoUser
handleAuth0Callback idpApp code = do
  mgr <- liftIO $ newManager tlsManagerSettings
  tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr code)
  Auth0User {..} <- withExceptT bslToText $ conduitUserInfoRequest idpApp mgr (accessToken tokenResp)
  pure (DemoUser name (Just email))

handleGoogleCallback ::
  IdpApplication Google AuthorizationCodeApplication ->
  ExchangeToken ->
  ExceptT TL.Text IO DemoUser
handleGoogleCallback idpApp code = do
  mgr <- liftIO $ newManager tlsManagerSettings
  tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr code)
  GoogleUser {..} <- withExceptT bslToText $ conduitUserInfoRequest idpApp mgr (accessToken tokenResp)
  pure (DemoUser name (Just email))

------------------------------

-- * Utilities

------------------------------

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
excepttToActionM :: Show a => ExceptT TL.Text IO a -> ActionM a
excepttToActionM e = do
  result <- liftIO $ runExceptT e
  either Scotty.raise pure result

oauth2ErrorToText :: TokenRequestError -> TL.Text
oauth2ErrorToText e = TL.pack $ "Unable fetch access token. error detail: " ++ show e
