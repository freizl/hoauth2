# questions

1. auto generate state if it's missing?

2. shall verify access token response header
  - Cache-Control: no-store
  - Pragma: no-cache

3. maybe an AuthFlow data type which would determine response type and grant type
  - or data type to determine auth request and access token request

4. character restriction for error fields.

> module Main where
>
> import GHC.Generic
>
> data ResponseType = Code
>                   | Token -- (maybe not support)
>                   deriving (Show, Eq, Generic)
>
> newtype AuthorizationCode = AuthorizationCode Text deriving (Show, Eq, Generic)
>
> type Scope = [Text]
> type State =  Text
>
> data AuthorizationCodeRequest = AuthorizationCodeReq
> { responseType :: ResponseType
> , clientId :: Text
> , redirectUri :: URI
> , scope :: Scope
> , state :: Maybe State
> } deriving (Show, Eq, Generic)
>
> data AuthorizationCodeResponse = AuthorizationCodeResponse
> { code :: AuthorizationCode
> , state :: Maybe State
> } deriving (Show, Eq, Generic)
>
> data AuthorizationCodeErrorCode = INVALID_REQUEST
>                                 | UNAUTHORIZED_CLIENT
>                                 | ACCESS_DENIED
>                                 | UNSUPPORTED_RESPONSE_TYPE
>                                 | INVALID_SCOPE
>                                 | SERVER_ERROR
>                                 | TEMPORARILY_UNAVAILABLE
>                                 deriving (Show, Eq, Generic)
>
> data AuthorizationCodeErrorResponse = AuthorizationCodeErrorResponse
> { error :: AuthorizationCodeErrorCode
> , errorDescription :: Maybe Text
> , errorUri :: Maybe URI
> , state :: Maybe State
> } deriving (Show, Eq, Generic)
>
> data GrantType = AUTHORIZATION_CODE
>                | PASSWORD
>                | REFRESH_TOKEN
>                deriving (Show, Eq, Generic)
>
>
> -- | Enum type for each flow and type class function to
> --   'convert' to auth code request parameters, body
> --                access token request parameters, body
> --
> -- 4.1 Auth code flow
> data AuthFlowAccessTokenRequest = AuthFlowAccessTokenRequest
> { grantType :: GrantType
> , code :: AuthorizationCode
> , redirectUri :: URI
> , clientId :: Text
> } deriving (Show, Eq, Generic)
>
> -- 4.2 Implicit flow
> data ImplicitFlowAccessTokenRequest = ImplicitFlowAccessTokenRequest
> { grantType :: GrantType
> , code :: AuthorizationCode
> , redirectUri :: URI
> , clientId :: Text
> } deriving (Show, Eq, Generic)
>

> -- 4.3 resource owner password credentials flow
> data AccessTokenRequest = AccessTokenRequest
> { grantType :: GrantType
> , username :: Text
> , password :: Text
> , scope :: Scope
> } deriving (Show, Eq, Generic)
>
> -- 4.4 Client Credentials Grant
> data AccessTokenRequest = AccessTokenRequest
> { grantType :: GrantType
> , scope :: Scope
> } deriving (Show, Eq, Generic)
>
> -- | https://tools.ietf.org/html/rfc6749#section-7.1
> data TokenType = TOKEN_TYPE_BEARER | TOKEN_TYPE_MAC deriving (Show, Eq, Generic)
>
> data AccessTokenResponse = AccessTokenResponse
> { accessToken :: AccessToken
> , tokenType :: TokenType
> , expiresIn :: Maybe Number
> , refreshToken :: Maybe RefreshToken
> , scope :: Scope
> } deriving (Show, Eq, Generic)
>
> data AccessTokenErrorResponeErrorCode = INVALID_REQUEST
>                                       | INVALID_CLIENT
>                                       | INVALID_GRANT
>                                       | UNAUTHORIZED_CLIENT
>                                       | UNSUPPORTED_GRANT_TYPE
>                                       | INVALID_SCOPE
>                                       deriving (Show, Eq, Generic)
>
> data AccessTokenErrorResponse = AccessTokenErrorResponse
> { error :: AccessTokenErrorResponeErrorCode
> , errorDescription :: Maybe Text
> , errorUri :: Maybe URI
> } deriving (Show, Eq, Generic)
>
> data RefreshTokenRequest = RefreshTokenRequest
> { grantType :: GrantType
> , refreshToken :: RefreshToken
> , scope :: Scope
> } deriving (Show, Eq, Generic)
