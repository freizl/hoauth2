module Network.OAuth.OAuth2.TokenRequestSpec where

import Data.Aeson qualified as Aeson
import Network.OAuth.OAuth2.TokenRequest
import Test.Hspec

spec :: Spec
spec =
  describe "parseJSON TokenRequestErrorCode" $ do
    it "invalid_request" $ do
      Aeson.eitherDecode "\"invalid_request\"" `shouldBe` Right InvalidRequest
    it "invalid_client" $ do
      Aeson.eitherDecode "\"invalid_client\"" `shouldBe` Right InvalidClient
    it "invalid_grant" $ do
      Aeson.eitherDecode "\"invalid_grant\"" `shouldBe` Right InvalidGrant
    it "unauthorized_client" $ do
      Aeson.eitherDecode "\"unauthorized_client\"" `shouldBe` Right UnauthorizedClient
    it "unsupported_grant_type" $ do
      Aeson.eitherDecode "\"unsupported_grant_type\"" `shouldBe` Right UnsupportedGrantType
    it "invalid_scope" $ do
      Aeson.eitherDecode "\"invalid_scope\"" `shouldBe` Right InvalidScope
    it "foo_code" $ do
      Aeson.eitherDecode "\"foo_code\"" `shouldBe` Right (UnknownErrorCode "foo_code")
