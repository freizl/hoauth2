module Network.OAuth2.Experiment.Flows.AuthorizationRequest where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Network.OAuth2.Experiment.Types

-------------------------------------------------------------------------------
--                           Authorization Request                           --
-------------------------------------------------------------------------------

data AuthorizationRequestParam = AuthorizationRequestParam
  { arScope :: Set Scope
  , arState :: AuthorizeState
  , arClientId :: ClientId
  , arRedirectUri :: Maybe RedirectUri
  , arResponseType :: ResponseType
  -- ^ It could be optional there is only one redirect_uri registered.
  -- See: https://www.rfc-editor.org/rfc/rfc6749#section-3.1.2.3
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
