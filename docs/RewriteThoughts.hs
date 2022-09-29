{-# LANGUAGE DuplicateRecordFields #-}
module RewriteThoughts where

import GHC.Generics
import Data.Text

-- | import URI.ByteString
data URI = URI
  deriving (Show, Eq, Generic)

-- | May not need to support Token
--
data ResponseType = Code | Token | IdToken
  deriving (Show, Eq, Generic)
newtype State = State Text
  deriving (Show, Eq, Generic)
newtype Nonce = Nonce Text
  deriving (Show, Eq, Generic)
newtype AuthorizationCode = AuthorizationCode Text
  deriving (Show, Eq, Generic)
newtype RefreshToken = RefreshToken Text
  deriving (Show, Eq, Generic)
newtype AccessToken = AccessToken Text
  deriving (Show, Eq, Generic)
data Scope = OpenID | OfflineAccess | Profile | Email | Custom Text
  deriving (Show, Eq, Generic)

data AuthorizationCodeRequest = AuthorizationCodeReq
  { responseType :: ResponseType
  , clientId :: Text
  , redirectUri :: Text
  , scope :: [ Scope ]
  , state :: State
  }
  deriving (Show, Eq, Generic)

data AuthorizationCodeSuccessResponse = AuthorizationCodeSuccessResponse
  { code :: AuthorizationCode
  , state :: State
  }
  deriving (Show, Eq, Generic)

data AuthorizationCodeErrorCode = CODE_INVALID_REQUEST
                                | CODE_UNAUTHORIZED_CLIENT
                                | CODE_ACCESS_DENIED
                                | CODE_UNSUPPORTED_RESPONSE_TYPE
                                | CODE_INVALID_SCOPE
                                | CODE_SERVER_ERROR
                                | CODE_TEMPORARILY_UNAVAILABLE
                                deriving (Show, Eq, Generic)

data AuthorizationCodeErrorResponse = AuthorizationCodeErrorResponse
  { error :: AuthorizationCodeErrorCode
  , errorDescription :: Maybe Text
  , errorUri :: Maybe URI
  , state :: Maybe State
  }
  deriving (Show, Eq, Generic)

type AuthorizationCodeResponse = Either AuthorizationCodeErrorResponse AuthorizationCodeSuccessResponse

data GrantType = AUTHORIZATION_CODE
               | PASSWORD
               | REFRESH_TOKEN
               deriving (Show, Eq, Generic)
-- | Enum type for each flow and type class function to
--   'convert' to auth code request parameters, body
--                access token request parameters, body
--
-- 4.1 Auth code flow
data CodeFlowAccessTokenRequest = CodeFlowAccessTokenRequest
  { grantType :: GrantType
  , code :: AuthorizationCode
  , redirectUri :: URI
  , clientId :: Text
  } deriving (Show, Eq, Generic)

-- 4.2 Implicit flow (do not support)

-- 4.3 resource owner password credentials flow
data ResourceOwenerAccessTokenRequest = ResourceOwenerAccessTokenRequest
  { grantType :: GrantType
  , username :: Text
  , password :: Text
  , scope :: [ Scope ]
  } deriving (Show, Eq, Generic)

-- 4.4 Client Credentials Grant
data ClientCredentialsAccessTokenRequest = ClientCredentialsAccessTokenRequest
  { grantType :: GrantType
  , scope :: [ Scope ]
  }
  deriving (Show, Eq, Generic)


-- | https://tools.ietf.org/html/rfc6749#section-7.1
data TokenType = TOKEN_TYPE_BEARER | TOKEN_TYPE_MAC deriving (Show, Eq, Generic)

-- | Looks like AccessTokenRequest can be GADTs to support 4.1, 4.2, 4.3?
data AccessTokenRequest = AccessTokenRequest
  { grantType :: GrantType
  , code :: AuthorizationCode
  , redirectUri :: URI
  , clientId :: Text
  }
  deriving (Show, Eq, Generic)

data AccessTokenSuccessResponse = AccessTokenSuccessResponse
  { accessToken :: AccessToken
  , tokenType :: Maybe Text
  , expiresIn :: Maybe Int
  , refreshToken :: Maybe RefreshToken
  -- , example_parameter :: Maybe Text -- ^ what is this??
  }
  deriving (Show, Eq, Generic)

data AccessTokenErrorResponeErrorCode = TOKEN_INVALID_REQUEST
                                      | TOKEN_INVALID_CLIENT
                                      | TOKEN_INVALID_GRANT
                                      | TOKEN_UNAUTHORIZED_CLIENT
                                      | TOKEN_UNSUPPORTED_GRANT_TYPE
                                      | TOKEN_INVALID_SCOPE
                                      deriving (Show, Eq, Generic)

data AccessTokenErrorResponse = AccessTokenErrorResponse
  { error :: AccessTokenErrorResponeErrorCode
  , errorDescription :: Maybe Text
  , errorUri :: Maybe URI
  }
  deriving (Show, Eq, Generic)

type AccessTokenResponse = Either AccessTokenErrorResponse AccessTokenSuccessResponse

data RefreshTokenRequest = RefreshTokenRequest
  { grantType :: GrantType
  , refreshToken :: RefreshToken
  , scope :: [ Scope ]
  }
  deriving (Show, Eq, Generic)

data RefreshTokenErrorResponse
data RefreshTokenSuccessResponse
data RefreshTokenResponse = Either RefreshTokenErrorResponse RefreshTokenSuccessResponse