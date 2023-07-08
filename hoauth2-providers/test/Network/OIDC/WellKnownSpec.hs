-- |

module Network.OIDC.WellKnownSpec where

import Data.Aeson qualified as Aeson
import Network.OIDC.WellKnown (OpenIDConfiguration(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseJSON OpenIDConfiguration" $ do
    it "parse openidConfiguration response" $ do
      let response = "{" <>
                      "\"issuer\": \"https://foo.com\"," <>
                      "\"authorization_endpoint\": \"https://foo.com/auth\"," <>
                      "\"token_endpoint\": \"https://foo.com/token\"," <>
                      "\"userinfo_endpoint\": \"https://foo.com/userinfo\"," <>
                      "\"jwks_uri\": \"https://foo.com/jwks\"" <>
                     "}"
      let expected = OpenIDConfiguration { issuer  = "https://foo.com"
                                         , authorizationEndpoint = "https://foo.com/auth"
                                         , tokenEndpoint = "https://foo.com/token"
                                         , userinfoEndpoint = "https://foo.com/userinfo"
                                         , jwksUri = "https://foo.com/jwks"
                                         }
      Aeson.eitherDecode response `shouldBe` Right expected
