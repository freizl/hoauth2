{- mimic server side session store -}

module Session where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import Data.HashMap.Strict qualified as Map
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Idp
import Network.OAuth.OAuth2
import Network.OAuth2.Experiment
import Text.Mustache ((~>))
import Text.Mustache qualified as M
import Types
import User

newtype AuthorizationGrantUserStore = AuthorizationGrantUserStore (MVar (Map.HashMap IdpName IdpAuthorizationCodeAppSessionData))

data IdpAuthorizationCodeAppSessionData = IdpAuthorizationCodeAppSessionData
  { idpName :: IdpName
  , loginUser :: Maybe DemoLoginUser
  , oauth2Token :: Maybe OAuth2Token
  , authorizePkceCodeVerifier :: Maybe CodeVerifier
  , authorizeAbsUri :: TL.Text
  }

instance Default IdpAuthorizationCodeAppSessionData where
  def =
    IdpAuthorizationCodeAppSessionData
      { idpName = ""
      , loginUser = Nothing
      , oauth2Token = Nothing
      , authorizePkceCodeVerifier = Nothing
      , authorizeAbsUri = ""
      }

instance M.ToMustache IdpAuthorizationCodeAppSessionData where
  toMustache (IdpAuthorizationCodeAppSessionData {..}) = do
    let hasDeviceGrant = idpName `elem` ["okta", "github", "auth0", "azure-ad", "google"]
        hasClientCredentialsGrant = idpName `elem` ["okta", "auth0"]
        hasPasswordGrant = idpName `elem` ["okta", "auth0"]
    M.object
      [ "isLogin" ~> isJust loginUser
      , "user" ~> loginUser
      , "idpName" ~> idpName
      , "hasDeviceGrant" ~> hasDeviceGrant
      , "hasClientCredentialsGrant" ~> hasClientCredentialsGrant
      , "hasPasswordGrant" ~> hasPasswordGrant
      ]

-- For the sake of simplicity for this demo App,
-- I store user data in MVar in server side.
-- It means user session shared across browsers.
-- which simplify my testing cross browsers.
-- I am sure you don't want to this for your production services.
initUserStore ::
  IO AuthorizationGrantUserStore
initUserStore = do
  let allIdps = fmap (\idpName -> (idpName, def {idpName = idpName})) supportedIdps
  AuthorizationGrantUserStore <$> newMVar (Map.fromList allIdps)

insertCodeVerifier ::
  AuthorizationGrantUserStore ->
  IdpName ->
  Maybe CodeVerifier ->
  ExceptT TL.Text IO ()
insertCodeVerifier store idpName val = do
  sdata <- lookupAppSessionData store idpName
  let newData = sdata {authorizePkceCodeVerifier = val}
  liftIO $ upsertAppSessionData store idpName newData

upsertAppSessionData ::
  AuthorizationGrantUserStore ->
  IdpName ->
  IdpAuthorizationCodeAppSessionData ->
  IO ()
upsertAppSessionData (AuthorizationGrantUserStore store) idpName val = do
  m1 <- takeMVar store
  let m2 =
        if Map.member idpName m1
          then Map.adjust (const val) idpName m1
          else Map.insert idpName val m1
  putMVar store m2

allAppSessionData :: AuthorizationGrantUserStore -> IO [IdpAuthorizationCodeAppSessionData]
allAppSessionData (AuthorizationGrantUserStore store) = do
  m1 <- tryReadMVar store
  return $ maybe [] Map.elems m1

removeAppSessionData ::
  AuthorizationGrantUserStore ->
  IdpName ->
  IO ()
removeAppSessionData store idpName = do
  upsertAppSessionData store idpName (def {idpName = idpName})

lookupAppSessionData ::
  AuthorizationGrantUserStore ->
  IdpName ->
  ExceptT TL.Text IO IdpAuthorizationCodeAppSessionData
lookupAppSessionData (AuthorizationGrantUserStore store) idpName@(IdpName name) = do
  mm <- liftIO $ tryReadMVar store
  m1 <-
    except $
      maybe
        (Left "[lookupAppSessionData] store (mvar) is empty")
        Right
        mm
  except $
    maybe
      (Left $ "[lookupAppSessionData] unable to find cache data for idp " <> name)
      Right
      (Map.lookup idpName m1)
