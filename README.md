[![Build](https://github.com/freizl/hoauth2/actions/workflows/build.yml/badge.svg)](https://github.com/freizl/hoauth2/actions/workflows/build.yml)
[![lint](https://github.com/freizl/hoauth2/actions/workflows/lint.yml/badge.svg)](https://github.com/freizl/hoauth2/actions/workflows/lint.yml)
<!-- [![Travis Status](https://app.travis-ci.com/freizl/hoauth2.svg?branch=master)](http://app.travis-ci.com/github/freizl/hoauth2) -->
[![Hackage](https://img.shields.io/hackage/v/hoauth2.svg)](https://hackage.haskell.org/package/hoauth2)

## Introduction

Lightweight Haskell binding for

- [The OAuth 2.0 Authorization Framework](https://datatracker.ietf.org/doc/html/rfc6749) 
    - Supports `Authorization Code Grant (section 4.1)`
    - TODO: Supports `Client Credentials Grant (section 4.4)`
    - If the provider does implement [OIDC spec](https://openid.net/specs/openid-connect-core-1_0.html),
      ID Token would also be included in token response (see `OAuth2Token`).
- [The OAuth 2.0 Authorization Framework: Bearer Token Usage](https://www.rfc-editor.org/rfc/rfc6750)

## Contribute

- Feel free send pull request or submit issue ticket.
