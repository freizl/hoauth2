module Network.OAuth2.Experiment.Flows.AuthorizationRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Network.OAuth.OAuth2 hiding (RefreshToken)
import Network.OAuth2.Experiment.CoreTypes
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Utils
import URI.ByteString hiding (UserInfo)

-------------------------------------------------------------------------------
--                           Authorization Request                           --
-------------------------------------------------------------------------------

data AuthorizationRequestParam = AuthorizationRequestParam
  { arScope :: Set Scope
  , arState :: AuthorizeState
  , arClientId :: ClientId
  , arRedirectUri :: Maybe RedirectUri -- TODO: why redirect uri is optional?
  , arResponseType :: ResponseType
  , arExtraParams :: Map Text Text
  }

instance ToQueryParam AuthorizationRequestParam where
  toQueryParam AuthorizationRequestParam {..} =
    Map.unions
      [ toQueryParam arResponseType
      , toQueryParam arScope
      , toQueryParam arClientId
      , toQueryParam arState
      , toQueryParam arRedirectUri
      , arExtraParams
      ]

class HasAuthorizeRequest a where
  mkAuthorizeRequestParam :: a -> AuthorizationRequestParam

mkAuthorizeRequest :: (HasAuthorizeRequest a) => IdpApplication a i -> Text
mkAuthorizeRequest idpApp =
  let req = mkAuthorizeRequestParam (application idpApp)
      allParams =
        map (bimap tlToBS tlToBS) $
          Map.toList $
            toQueryParam req
   in TL.fromStrict $
        T.decodeUtf8 $
          serializeURIRef' $
            appendQueryParams allParams $
              idpAuthorizeEndpoint (idp idpApp)

-------------------------------------------------------------------------------
--                                    PKCE                                   --
-------------------------------------------------------------------------------

class (HasAuthorizeRequest a) => HasPkceAuthorizeRequest a where
  mkPkceAuthorizeRequestParam :: (MonadIO m) => a -> m (AuthorizationRequestParam, CodeVerifier)

mkPkceAuthorizeRequest ::
  (HasPkceAuthorizeRequest a, MonadIO m) =>
  IdpApplication a i ->
  m (Text, CodeVerifier)
mkPkceAuthorizeRequest IdpApplication {..} = do
  (req, codeVerifier) <- mkPkceAuthorizeRequestParam application
  let allParams = map (bimap tlToBS tlToBS) $ Map.toList $ toQueryParam req
  let url =
        TL.fromStrict $
          T.decodeUtf8 $
            serializeURIRef' $
              appendQueryParams allParams $
                idpAuthorizeEndpoint idp
  pure (url, codeVerifier)
