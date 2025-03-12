{-# LANGUAGE OverloadedStrings #-}

module Network.OAuth.OAuth2.TokenResponseSpec where

import Data.Aeson qualified as Aeson
import Network.OAuth.OAuth2 (OAuth2Token(..), AccessToken(..), RefreshToken(..))
import Data.Maybe ( fromJust )
import Test.Hspec

spec :: Spec
spec = do
  describe "parseJSON TokenResponse" $ do
    it "parse access token" $ do
      let resp = "{\"access_token\":\"ya29\",\"token_type\":\"Bearer\",\"expires_in\":3600,\"refresh_token\":\"0gk\"}"
      Aeson.eitherDecode resp
        `shouldBe` Right
          ( OAuth2Token
              { accessToken= AccessToken "ya29"
              , refreshToken = Just (RefreshToken "0gk" )
              , expiresIn= Just 3600
              , tokenType= Just "Bearer"
              , idToken = Nothing
              , scope = Nothing
              , rawResponse = fromJust (Aeson.decode resp)
              }
          )
    it "parse access token with scope" $ do
      let resp = "{\"access_token\":\"ya29\",\"token_type\":\"Bearer\",\"expires_in\":3600,\"refresh_token\":\"0gk\",\"scope\": \"openid profile\"}"
      Aeson.eitherDecode resp
        `shouldBe` Right
          ( OAuth2Token
              { accessToken= AccessToken "ya29"
              , refreshToken = Just (RefreshToken "0gk" )
              , expiresIn= Just 3600
              , tokenType= Just "Bearer"
              , idToken = Nothing
              , scope = Just "openid profile"
              , rawResponse = fromJust (Aeson.decode resp)
              }
          )
