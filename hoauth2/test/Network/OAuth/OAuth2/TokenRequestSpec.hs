{-# LANGUAGE QuasiQuotes #-}

module Network.OAuth.OAuth2.TokenRequestSpec where

import Data.Aeson qualified as Aeson
import Network.OAuth.OAuth2.TokenRequest
import Test.Hspec
import URI.ByteString.QQ
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "parseJSON TokenResponseErrorCode" $ do
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

  describe "parseJSON TokenResponseError" $ do
    it "parse error" $ do
      Aeson.eitherDecode "{\"error\": \"invalid_request\"}"
        `shouldBe` Right
          ( TokenResponseError
              { error = InvalidRequest
              , errorDescription = Nothing
              , errorUri = Nothing
              }
          )
    it "parse error_description" $ do
      Aeson.eitherDecode "{\"error\": \"invalid_request\", \"error_description\": \"token request error foo1\"}"
        `shouldBe` Right
          ( TokenResponseError
              { error = InvalidRequest
              , errorDescription = Just "token request error foo1"
              , errorUri = Nothing
              }
          )
    it "parse error_uri" $ do
      Aeson.eitherDecode "{\"error\": \"invalid_request\", \"error_uri\": \"https://example.com\"}"
        `shouldBe` Right
          ( TokenResponseError
              { error = InvalidRequest
              , errorDescription = Nothing
              , errorUri = Just [uri|https://example.com|]
              }
          )
