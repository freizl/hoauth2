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
import Network.OAuth2.Experiment.Pkce
import Network.OAuth2.Experiment.Types
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
  -- | Constructs Authorization Code request parameters
  -- | https://www.rfc-editor.org/rfc/rfc6749#section-4.1.1
  mkAuthorizeRequestParam :: a -> AuthorizationRequestParam

-- FIXME: rename to mkAuthorizationRequest. similar to other cases.

-- | Constructs Authorization Code request URI
-- https://www.rfc-editor.org/rfc/rfc6749#section-4.1.1
mkAuthorizeRequest :: HasAuthorizeRequest a => IdpApplication i a -> Text
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

-- | https://datatracker.ietf.org/doc/html/rfc7636
class HasAuthorizeRequest a => HasPkceAuthorizeRequest a where
  mkPkceAuthorizeRequestParam :: MonadIO m => a -> m (AuthorizationRequestParam, CodeVerifier)

-- | Constructs Authorization Code (PKCE) request URI and the Code Verifier.
-- https://datatracker.ietf.org/doc/html/rfc7636
mkPkceAuthorizeRequest ::
  (HasPkceAuthorizeRequest a, MonadIO m) =>
  IdpApplication i a ->
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
