# hoauth2-provider: Pre-configured OAuth2 Providers

This package provides ready-to-use configurations and helper functions for popular OAuth2 and OpenID Connect providers.

## Supported Providers

| Provider  | OpenID Connect | Features |
|----------|----------------|-----------|
| Auth0    | ✓ | Authorization Code, Client Credentials, PKCE |
| Azure AD | ✓ | Authorization Code, Client Credentials, Device Flow |
| Google   | ✓ | Authorization Code, PKCE |
| GitHub   | × | Authorization Code, Device Flow |
| Okta     | ✓ | Authorization Code, Client Credentials, Device Flow |

## Usage Examples

### Auth0

```haskell
import Network.OAuth2.Provider.Auth0
import Network.OAuth2.Experiment

-- Create Auth0 IdP configuration
auth0App :: ExceptT Text IO (IdpApplication Auth0 AuthorizationCodeApplication)
auth0App = do
    idp <- mkAuth0Idp "your-tenant.auth0.com"
    let application = AuthorizationCodeApplication {
            acClientId = "your_client_id",
            acClientSecret = "your_client_secret",
            acScope = Set.fromList ["openid", "profile", "email"],
            acRedirectUri = [uri|http://localhost:3000/callback|],
            acName = "my-auth0-app"
        }
    pure IdpApplication {..}
```

### Google

```haskell
import Network.OAuth2.Provider.Google

-- Create Google IdP application
googleApp :: IdpApplication Google AuthorizationCodeApplication
googleApp = IdpApplication {
    idp = defaultGoogleIdp,
    application = AuthorizationCodeApplication {
        acClientId = "your_client_id",
        acClientSecret = "your_client_secret",
        acScope = Set.fromList [
            "https://www.googleapis.com/auth/userinfo.email",
            "https://www.googleapis.com/auth/userinfo.profile"
        ],
        acRedirectUri = [uri|http://localhost:3000/callback|],
        acName = "my-google-app"
    }
}
```

## Provider-Specific Features

### Auth0
- Standard OAuth2 endpoints
- OIDC support
- User profile information
- Management API support

### Azure AD
- Multi-tenant support
- Device flow
- Microsoft Graph API integration
- B2C support

### Google
- Standard OAuth2 endpoints
- OIDC support
- Google API integration
- Application Default Credentials

### GitHub
- Standard OAuth2 endpoints
- Device flow
- GitHub API integration

### Okta
- Standard OAuth2 endpoints
- OIDC support
- Multi-tenant support
- Authorization server support

## Common Provider Functions

Each provider module includes:

1. Configuration Functions
    - `mkXXXIdp`: Create provider-specific IdP configuration
    - Default endpoint configurations

2. User Profile Types
    - Provider-specific user information types
    - JSON instances for API responses

3. Helper Functions
    - Token request helpers
    - User information fetching
    - API integration utilities

## Contributing

To add support for a new provider:

1. Add a new module under `Network.OAuth2.Provider`
2. Implement the core provider types and configurations
3. Add user profile types and JSON instances
4. Include helper functions for common operations
5. Add examples and documentation
6. Update this README with the new provider's details

## Security Notes

- Keep client credentials secure
- Use HTTPS for all provider endpoints
- Implement proper state validation
- Use PKCE when supported
- Follow each provider's security best practices

## See Also

- [hoauth2](../hoauth2) - Core OAuth2 functionality
- [hoauth2-tutorial](../hoauth2-tutorial) - Basic usage tutorial
- [hoauth2-providers-tutorial](../hoauth2-providers-tutorial) - Provider-specific examples
