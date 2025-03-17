# hoauth2: Haskell OAuth2 Bindings

A comprehensive Haskell binding for OAuth2 protocols and extensions.

## Supported Specifications

* [The OAuth 2.0 Authorization Framework](https://datatracker.ietf.org/doc/html/rfc6749)
    - Authorization Code Grant
    - Client Credentials Grant
    - Resource Owner Password Grant
    - Device Authorization Grant
    - PKCE Extension ([RFC 7636](https://datatracker.ietf.org/doc/html/rfc7636))
* [OpenID Connect Core](https://openid.net/specs/openid-connect-core-1_0.html)
    - ID Token handling in token responses
    - UserInfo endpoint integration
* [JWT Profile for OAuth2](https://www.rfc-editor.org/rfc/rfc7523.html)
    - Client Authentication
    - Authorization Grants
* [Bearer Token Usage](https://www.rfc-editor.org/rfc/rfc6750)

## Quick Start

Add the following to your project's cabal file:
```cabal
build-depends: hoauth2 >= 2.14.2
```

Basic usage example:
```haskell
import Network.OAuth.OAuth2

-- Configure OAuth2 settings
oauth2 = OAuth2 
    { oauth2ClientId = "your_client_id"
    , oauth2ClientSecret = "your_client_secret"
    , oauth2AuthorizeEndpoint = [uri|https://provider.com/authorize|]
    , oauth2TokenEndpoint = [uri|https://provider.com/token|]
    , oauth2RedirectUri = [uri|http://localhost:3000/callback|]
    }

-- Generate authorization URL
let authUrl = appendQueryParams 
      [ ("scope", "openid profile email")
      , ("state", "random-state-value")
      ] $ authorizationUrl oauth2

-- Exchange authorization code for token
mgr <- newManager tlsManagerSettings
let code = ExchangeToken "auth_code_from_callback"
tokenResponse <- fetchAccessToken mgr oauth2 code
```

## Documentation

* See [hoauth2-tutorial](./hoauth2-tutorial) for a complete example
* See [hoauth2-providers](./hoauth2-providers) for pre-configured provider support
* Check [Haddock documentation](https://hackage.haskell.org/package/hoauth2) for API details

## Package Organization

* `Network.OAuth.OAuth2`: Main module exposing core functionality
* `Network.OAuth.OAuth2.AuthorizationRequest`: Authorization request handling
* `Network.OAuth.OAuth2.TokenRequest`: Token request and response handling
* `Network.OAuth.OAuth2.HttpClient`: OAuth-authenticated HTTP client utilities

## Version Compatibility

| GHC Version | Package Version | Notes |
|------------|-----------------|-------|
| 9.6        | >= 2.14.2      | Current |
| 9.4        | >= 2.8.0       | Supported |
| 9.2        | >= 2.6.0       | Supported |

## Security Considerations

1. Always validate the `state` parameter in OAuth callbacks
2. Use PKCE for public clients
3. Store tokens securely
4. Use HTTPS for all endpoints
5. Keep client secrets secure

## Related Packages

* `hoauth2-provider`: Pre-configured support for popular OAuth2 providers
* `hoauth2-tutorial`: Step-by-step tutorial implementation
* `hoauth2-providers-tutorial`: Examples for specific provider integrations

## License

MIT
