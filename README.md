[![Build Status](https://secure.travis-ci.org/freizl/hoauth2.svg?branch=master)](http://travis-ci.org/freizl/hoauth2)
[![Hackage](https://img.shields.io/hackage/v/hoauth2.svg)](https://hackage.haskell.org/package/hoauth2)

# Introduction

A lightweight OAuth2 Haskell binding.

# Build example app

- `make create-keys`
- check the `example/Keys.hs` to make sure it's config correctly for the IdP you're going to test. (client id, client secret, oauth Urls etc)
- `make build`
- `make demo`
- open <http://localhost:9988>

## Nix

- assume `cabal-install` has been install (either globally or in nix store)
- `nix-shell` then could do `cabal v2-` build
- or `nix-build`

# Contribute

Feel free send pull request or submit issue ticket.
