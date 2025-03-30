{-# LANGUAGE QuasiQuotes #-}

module Network.OAuth.OAuth2.InternalSpec where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as Aeson
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Test.Hspec
import URI.ByteString.QQ

spec :: Spec
spec = do
  describe "uriToRequest" $ do
    it "parse http://localhost:3001/abc/foo?scope=openid&prompt=consent" $ do
      req <- liftIO $ uriToRequest [uri|http://localhost:3001/abc/foo?scope=openid&prompt=consent|]
      secure req `shouldBe` False
      path req `shouldBe` "/abc/foo"
      port req `shouldBe` 3001
      host req `shouldBe` "localhost"
      queryString req `shouldBe` "?scope=openid&prompt=consent"
    it "parse http://localhost:3001/abc/foo" $ do
      req <- liftIO $ uriToRequest [uri|http://localhost:3001/abc/foo|]
      secure req `shouldBe` False
      path req `shouldBe` "/abc/foo"
      port req `shouldBe` 3001
      host req `shouldBe` "localhost"
      queryString req `shouldBe` ""
    it "parse http://localhost:3001/" $ do
      req <- liftIO $ uriToRequest [uri|http://localhost:3001/|]
      secure req `shouldBe` False
      path req `shouldBe` "/"
      port req `shouldBe` 3001
      host req `shouldBe` "localhost"
      queryString req `shouldBe` ""
    it "parse https://test.auth0.com/authorize" $ do
      req <- liftIO $ uriToRequest [uri|https://test.auth0.com/authorize|]
      secure req `shouldBe` True
      path req `shouldBe` "/authorize"
      port req `shouldBe` 443
      host req `shouldBe` "test.auth0.com"
      queryString req `shouldBe` ""
