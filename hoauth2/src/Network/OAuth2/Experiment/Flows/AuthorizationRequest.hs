module Network.OAuth2.Experiment.Flows.AuthorizationRequest where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
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
  , arRedirectUri :: Maybe RedirectUri
  -- | It could be optional there is only one redirect_uri registered.
  -- See: https://www.rfc-editor.org/rfc/rfc6749#section-3.1.2.3
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
  mkAuthorizationRequestParam :: a -> AuthorizationRequestParam

-- | Constructs Authorization Code request URI
-- https://www.rfc-editor.org/rfc/rfc6749#section-4.1.1
mkAuthorizationRequest :: HasAuthorizeRequest a => IdpApplication i a -> URI
mkAuthorizationRequest idpApp =
  let req = mkAuthorizationRequestParam (application idpApp)
      allParams =
        map (bimap tlToBS tlToBS) $
          Map.toList $
            toQueryParam req
   in appendQueryParams allParams $
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
  (MonadIO m, HasPkceAuthorizeRequest a) =>
  IdpApplication i a ->
  m (URI, CodeVerifier)
mkPkceAuthorizeRequest IdpApplication {..} = do
  (req, codeVerifier) <- mkPkceAuthorizeRequestParam application
  let allParams = map (bimap tlToBS tlToBS) $ Map.toList $ toQueryParam req
  let url =
        appendQueryParams allParams $
          idpAuthorizeEndpoint idp
  pure (url, codeVerifier)
