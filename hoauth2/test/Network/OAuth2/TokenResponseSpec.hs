{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.OAuth2.TokenResponseSpec where

import Data.Aeson qualified as Aeson
import Data.Binary qualified as Binary
import Data.Maybe (fromJust)
import Network.OAuth2 (AccessToken (..), RefreshToken (..), TokenResponse (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "decode as JSON" $ do
    it "parse access token" $ do
      let resp = "{\"access_token\":\"ya29\",\"token_type\":\"Bearer\",\"expires_in\":3600,\"refresh_token\":\"0gk\"}"
      Aeson.eitherDecode resp
        `shouldBe` Right
          ( TokenResponse
              { accessToken = AccessToken "ya29"
              , refreshToken = Just (RefreshToken "0gk")
              , expiresIn = Just 3600
              , tokenType = Just "Bearer"
              , idToken = Nothing
              , scope = Nothing
              , rawResponse = fromJust (Aeson.decode resp)
              }
          )
    it "parse access token with scope" $ do
      let resp = "{\"access_token\":\"ya29\",\"token_type\":\"Bearer\",\"expires_in\":3600,\"refresh_token\":\"0gk\",\"scope\": \"openid profile\"}"
      Aeson.eitherDecode resp
        `shouldBe` Right
          ( TokenResponse
              { accessToken = AccessToken "ya29"
              , refreshToken = Just (RefreshToken "0gk")
              , expiresIn = Just 3600
              , tokenType = Just "Bearer"
              , idToken = Nothing
              , scope = Just "openid profile"
              , rawResponse = fromJust (Aeson.decode resp)
              }
          )
  describe "encode/decode binary" $ do
    it "support binary encoding" $ do
      let resp = "{\"access_token\":\"ya29\",\"token_type\":\"Bearer\",\"expires_in\":3600,\"refresh_token\":\"0gk\"}"
          oauth2Token = fromJust (Aeson.decode @TokenResponse resp)
      Binary.decode @TokenResponse (Binary.encode oauth2Token)
        `shouldBe` oauth2Token
