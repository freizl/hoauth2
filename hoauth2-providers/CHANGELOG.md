# hoauth2-provider Changelog

## 0.9.0 (2025-10-05)

- Add Linear provider
- Fix `OktaUser` default values; parameterize StackExchange key
- Update Facebook and LinkedIn configurations
- Allow `uri-bytestring`-0.4 and `microlens`-0.5
- Internal refactors and cleanups

## 0.8.0 (2024-11-19)

* Updated crypton dependency to version 1.0

## 0.7.0 (2024-03-07)

* Replaced cryptonite dependency with crypton

## 0.6.0 (2024-01-19)

* Updated base64 dependency to version 1.0

## 0.5.0 (2023-12-30)

* Updated aeson dependency to version 2.2
* Updated binary dependency to version 0.10
* Updated bytestring dependency to version 0.12
* Updated container dependency to version 0.7

## 0.4.0 (2023-11-17)

* Updated text dependency to version 2.2

## 0.3.0 (2023-10-26)

* Added mkAzureADIdp helper function
* Renamed Experiment.GrantType to Experiment.Grants
* Added capability to fetch deviceAuthorizationEndpoint from OIDC configuration
* Added generator for all IdP names
* Added IdP-initiated login flow demo for Okta

## 0.2.0 (2022-11-17)

* Updated endpoint domain for Azure IdP
* Renamed `DropboxName` to `DropboxUserName`
