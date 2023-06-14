{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HOAuth2ProvidersTutorial where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network.HTTP.Types (status302)
import Network.OAuth.OAuth2 (
  ExchangeToken (ExchangeToken),
  OAuth2Token (accessToken),
  TokenRequestError,
 )
import Network.OAuth2.Experiment.Types
import Network.OAuth2.Experiment.Flows.AuthorizationRequest
import Network.OAuth2.Experiment.Flows.TokenRequest
import Network.OAuth2.Experiment.Flows.UserInfoRequest
import Network.OAuth2.Experiment.GrantType.AuthorizationCode (Application (..))
import Network.OAuth2.Experiment.GrantType.AuthorizationCode qualified as AuthorizationCode
import Network.OAuth2.Provider.Auth0 (Auth0, Auth0User (..))
import Network.OAuth2.Provider.Auth0 qualified as Auth0
import Network.OAuth2.Provider.Google (Google, GoogleUser (..))
import Network.OAuth2.Provider.Google qualified as Google
import URI.ByteString (URI, serializeURIRef')
import URI.ByteString.QQ (uri)
import Web.Scotty (ActionM, scotty)
import Web.Scotty qualified as Scotty
import Prelude hiding (id)

------------------------------

-- * Configuration

------------------------------

testAuth0App :: IdpApplication AuthorizationCode.Application Auth0
testAuth0App =
  let application =
        Auth0.defaultAuth0App
          { acClientId = ""
          , acClientSecret = ""
          , acAuthorizeState = AuthorizeState ("auth0." <> randomStateValue)
          , acScope = Set.fromList ["openid", "email", "profile"]
          , acRedirectUri = [uri|http://localhost:9988/oauth2/callback|]
          , acName = "foo-auth0-app"
          }
      idp = testAuth0Idp
   in IdpApplication {..}

testAuth0Idp :: Idp Auth0
testAuth0Idp =
  Auth0.defaultAuth0Idp
    { idpUserInfoEndpoint = [uri|https://freizl.auth0.com/userinfo|]
    , idpAuthorizeEndpoint = [uri|https://freizl.auth0.com/authorize|]
    , idpTokenEndpoint = [uri|https://freizl.auth0.com/oauth/token|]
    }

testGoogleApp :: IdpApplication AuthorizationCode.Application Google
testGoogleApp =
  let application =
        Google.defaultGoogleApp
          { acClientId = ""
          , acClientSecret = ""
          , acAuthorizeState = AuthorizeState ("google." <> randomStateValue)
          , acRedirectUri = [uri|http://localhost:9988/oauth2/callback|]
          , acName = "foo-google-app"
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
  -- Poor man's solution for creating user session.
  refUser <- newIORef Nothing
  scotty 9988 $ do
    Scotty.get "/" $ indexH refUser
    Scotty.get "/login/auth0" loginAuth0H
    Scotty.get "/login/google" loginGoogleH
    Scotty.get "/logout" (logoutH refUser)
    Scotty.get "/oauth2/callback" $ callbackH refUser

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
loginAuth0H :: ActionM ()
loginAuth0H = do
  Scotty.setHeader "Location" (mkAuthorizeRequest testAuth0App)
  Scotty.status status302

-- | @/login/google@ endpoint handler
loginGoogleH :: ActionM ()
loginGoogleH = do
  Scotty.setHeader "Location" (mkAuthorizeRequest testGoogleApp)
  Scotty.status status302

-- | @/logout@ endpoint handler
logoutH :: IORef (Maybe DemoUser) -> ActionM ()
logoutH refUser = do
  liftIO (writeIORef refUser Nothing)
  Scotty.redirect "/"

-- | @/oauth2/callback@ endpoint handler
callbackH :: IORef (Maybe DemoUser) -> ActionM ()
callbackH refUser = do
  pas <- Scotty.params

  excepttToActionM $ do
    state <- ExceptT $ pure $ paramValue "state" pas
    codeP <- ExceptT $ pure $ paramValue "code" pas

    let code = ExchangeToken $ TL.toStrict codeP
    let idpName = TL.takeWhile ('.' /=) state

    user <- case idpName of
      "google" -> handleGoogleCallback code
      "auth0" -> handleAuth0Callback code
      _ -> throwE $ "unable to find idp app of: " <> idpName

    liftIO $ writeIORef refUser (Just user)

  Scotty.redirect "/"

handleAuth0Callback :: ExchangeToken -> ExceptT TL.Text IO DemoUser
handleAuth0Callback code = do
  let idpApp = testAuth0App
  mgr <- liftIO $ newManager tlsManagerSettings
  tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr (Just code))
  Auth0User {..} <- withExceptT bslToText $ conduitUserInfoRequest idpApp mgr (accessToken tokenResp)
  pure (DemoUser name (Just email))

handleGoogleCallback :: ExchangeToken -> ExceptT TL.Text IO DemoUser
handleGoogleCallback code = do
  let idpApp = testGoogleApp
  mgr <- liftIO $ newManager tlsManagerSettings
  tokenResp <- withExceptT oauth2ErrorToText (conduitTokenRequest idpApp mgr (Just code))
  GoogleUser {..} <- withExceptT bslToText $ conduitUserInfoRequest idpApp mgr (accessToken tokenResp)
  pure (DemoUser name (Just email))

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
