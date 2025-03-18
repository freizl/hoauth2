{-# LANGUAGE QuasiQuotes #-}

module Network.OIDC.WellKnownSpec where

import Data.Aeson qualified as Aeson
import Network.OIDC.WellKnown (OpenIDConfiguration (..))
import Test.Hspec
import Network.URI.Static(uri)

spec :: Spec
spec = do
  describe "parseJSON OpenIDConfiguration" $ do
    it "parse openidConfiguration response" $ do
      let response =
            "{"
              <> "\"issuer\": \"https://foo.com\","
              <> "\"authorization_endpoint\": \"https://foo.com/auth\","
              <> "\"token_endpoint\": \"https://foo.com/token\","
              <> "\"userinfo_endpoint\": \"https://foo.com/userinfo\","
              <> "\"jwks_uri\": \"https://foo.com/jwks\","
              <> "\"device_authorization_endpoint\": \"https://foo.com/device-code\""
              <> "}"
      let expected =
            OpenIDConfiguration
              { issuer = [uri|https://foo.com|]
              , authorizationEndpoint = [uri|https://foo.com/auth|]
              , tokenEndpoint = [uri|https://foo.com/token|]
              , userinfoEndpoint = [uri|https://foo.com/userinfo|]
              , jwksUri = [uri|https://foo.com/jwks|]
              , deviceAuthorizationEndpoint = [uri|https://foo.com/device-code|]
              }
      Aeson.eitherDecode response `shouldBe` Right expected
