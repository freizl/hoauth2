[![Build](https://github.com/freizl/hoauth2/actions/workflows/build.yml/badge.svg)](https://github.com/freizl/hoauth2/actions/workflows/build.yml)
[![lint](https://github.com/freizl/hoauth2/actions/workflows/lint.yml/badge.svg)](https://github.com/freizl/hoauth2/actions/workflows/lint.yml)
[![Travis Status](https://app.travis-ci.com/freizl/hoauth2.svg?branch=master)](http://app.travis-ci.com/github/freizl/hoauth2)
[![Hackage](https://img.shields.io/hackage/v/hoauth2.svg)](https://hackage.haskell.org/package/hoauth2)

# Introduction

A lightweight OAuth2 Haskell binding.

# Build the sample App

- Make sure `ghc-8.10` and `cabal-3.x` installed.
- copy `sample.env.json` to `.env.json` and modify the clientId and clientSecret
- `make start-demo`
- open <http://localhost:9988>

## Nix (WIP)

- assume `cabal-install` has been install (either globally or in nix store)
- `nix-shell` then could do `cabal v2-` build
- or `nix-build`

# Contribute

Feel free send pull request or submit issue ticket.
