# hoauth2 Changelog

## 2.15.0 (2025-10-05)
- Breaking changes
  - Type and class renames
    - `OAuth2Token` renamed to `TokenResponse`.
    - `TokenRequestAuthenicationMethod` renamed to `ClientAuthenticationMethod`.
    - `HasTokenRequestClientAuthenticationMethod` renamed to `HasClientAuthenticationMethod`.
    - Removed several `Has*` type classes (e.g., `HasOAuth2Key`, `HasOAuthKey` from `HasTokenRequest`).
  - Removed deprecated API in `Network.OAuth2.HttpClient`
    - Deprecated functions have been removed. Use `authGetJSON`/`authGetBS`/`authPostJSON`/`authPostBS` and the `WithAuthMethod` variants with `APIAuthenticationMethod`.
  - Request body encoding behavior
    - `authPostJSON` now sends JSON with `Content-Type: application/json`. If an endpoint requires form-encoded data, use `authPostBS` or construct a custom request body.
  - Import/module path cleanup
    - Consumers should import `Network.OAuth2` (not `Network.OAuth.OAuth2`).
- Behavioral changes and bug fixes
  - Client authentication handling
    - Append `client_id` and `client_secret` for `ClientSecretPost` in client credentials flow.
    - Refresh token flow honors the configured client authentication method.
    - Authorization Code flow supports client authentication methods.
  - Device Authorization flow
    - Updated handling of device authorization requests and polling behavior.
- Refactors and internal cleanup
  - Refactored header handling, simplified URI-to-Request conversion, removed deprecated or unused internals, and general cleanups.
- Dependency changes
  - Allow `microlens` 0.5 (`microlens` >= 0.4 && < 0.6).
  - Raise lower bound to `uri-bytestring` >= 0.4 (< 0.5).

## 2.14.3 (2025-03-14)

- Fixes and improvements:
  - Append client_id and client_secret for ClientSecretPost in client credentials flow
  - Add raw response to token response and derive Show instances
  - Skip empty scope; assorted refactors and cleanups

## 2.14.2 (2025-01-30)

* Updated uri-bytestring to version 0.4

## 2.14.1 (2024-11-19)

* Updated data-default to version 0.8

## 2.14.0 (2024-11-19)

* Updated crypton to version 1.0

## 2.13.0 (2024-03-07)

* Replaced cryptonite with crypton

## 2.12.0 (2024-01-19)

* Updated base64 to version 1.0

## 2.11.0 (2023-12-30)

* Updated aeson to version 2.2
* Updated binary to version 0.10
* Updated bytestring to version 0.12
* Updated container to version 0.7

## 2.10.0 (2023-11-17)

* Added support for text 2.2

## 2.9.0 (2023-10-26)

* Refactored oauth2 Experiment module implementation
* Removed generics
* Added Device Authorization grant
* Moved IdpName to hoauth2 and enabled DataKinds
* Changed the type parameter in HttpClient methods
* Removed data family `RefreshTokenRequest`

## 2.8.1 (2023-06-17)

* Added support for GHC-9.6
* Updated CI configuration for GHC 9.6.1
* Added hiedb integration

## 2.8.0 (2023-03-15)

* Added support for GHC-9.4.4
* Added support for text-2.0

## 2.7 (2022-11-17)

* Added GrantType jwt-bearer
* Added JWT authentication method for ClientCredential flow
* Replaced `OAuth2Error` with `TokenRequestError`
* Moved `HasIdpName` class to hoauth2-demo
* Improved error handling when response body is empty in `HttpClient.handleResponse`
* Removed the following modules from exposure list:
    - Network.OAuth2.Experiment.Pkce
    - Network.OAuth2.Experiment.Types
    - Network.OAuth2.Experiment.Utils
    - Network.OAuth.OAuth2.Internal

## 2.6 (2022-10-04)

* Changed type parameter order in http client JSON method
* Modified http client to only accept one Authentication Method instead of a Set
* Removed `authPostBS1` (non-standard approach to sending credentials)
* Removed Douban IdP (discontinued OAuth2 support)
* Deprecated all *Internal methods, added *WithAuthMethod alternatives
* Changed license to MIT
* Added support for PKCE flow in `Network.OAuth2.Experiment` module
* Added support for Resource Owner Password and Client Credentials flows
* Added `hoauth2-providers` and `hoauth2-providers-tutorial` modules
* Added `hoauth2-tutorial` module

## 2.5 (2022-08-17)

* Updated aeson to version 2.1

## 2.4 (2022-08-17)

* Relaxed binary and bytestring version constraints

## 2.1 (2022-02-19)

* Added documentation for OAuth2 specification
* Updated aeson to version 2

## 2.0 (2022-02-15)

* Breaking change: Refactored naming convention of `OAuth2` data type
  ```diff
  -  { oauthClientId            = "xxxxxxxxxxxxxxx"
  -  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  -  , oauthCallback            = Just [uri|http://127.0.0.1:9988/oauthCallback|]
  -  , oauthOAuthorizeEndpoint  = [uri|https://api.weibo.com/oauth2/authorize|]
  -  , oauthAccessTokenEndpoint = [uri|https://api.weibo.com/oauth2/access_token|]
  +  { oauth2ClientId           = "xxxxxxxxxxxxxxx"
  +  , oauth2ClientSecret       = Just "xxxxxxxxxxxxxxxxxxxxxx"
  +  , oauth2RedirectUri        = Just [uri|http://127.0.0.1:9988/oauthCallback|]
  +  , oauth2AuthorizeEndpoint  = [uri|https://api.weibo.com/oauth2/authorize|]
  +  , oauth2TokenEndpoint      = [uri|https://api.weibo.com/oauth2/access_token|]
  +  }
  ```

## 1.7.0 (2018-03-03)

* Added sample server and removed individual sample tests
* Added `fetchAccessToken2` function
* Added `refreshAccessToken` function and deprecated `fetchRefreshToken`
* Renamed `authGetBS'` to `authGetBS2`
* Renamed `authPostBS'` to `authPostBS2`
* Added `authPostBS3` function

## 1.0.0 (2017-04-07)

* Added umbrella type `OAuth2Token` to accommodate `AccessToken` and `RefreshToken`
* Typed the intermediate authentication code as `ExchangeToken`
* Fixed missing client_id error in tests by appending client_id and client_secret to header
