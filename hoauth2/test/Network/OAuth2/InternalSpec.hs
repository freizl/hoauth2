{-# LANGUAGE QuasiQuotes #-}

module Network.OAuth2.InternalSpec where

import Control.Monad.IO.Class (MonadIO (..))
import Network.HTTP.Conduit
import Network.OAuth2
import Test.Hspec
import URI.ByteString.QQ

spec :: Spec
spec = do
  describe "uriToRequest" $ do
    it "parse http://localhost:3001/abc/foo?scope=openid&redirect_uri=http://localhost:3001/callback" $ do
      req <- liftIO $ uriToRequest [uri|http://localhost:3001/abc/foo?scope=openid&&redirect_uri=http://localhost:3001/callback|]
      secure req `shouldBe` False
      path req `shouldBe` "/abc/foo"
      port req `shouldBe` 3001
      host req `shouldBe` "localhost"
      queryString req `shouldBe` "?scope=openid&redirect_uri=http%3A%2F%2Flocalhost%3A3001%2Fcallback"
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
    it "parse URL with multiple query parameters" $ do
      req <- liftIO $ uriToRequest [uri|https://api.example.com/oauth2/auth?client_id=123&scope=read,write&state=xyz|]
      secure req `shouldBe` True
      path req `shouldBe` "/oauth2/auth"
      port req `shouldBe` 443
      host req `shouldBe` "api.example.com"
      queryString req `shouldBe` "?client_id=123&scope=read%2Cwrite&state=xyz"
    it "parse URL with special characters in path" $ do
      req <- liftIO $ uriToRequest [uri|https://api.example.com/oauth2/user+info/profile%20data|]
      secure req `shouldBe` True
      path req `shouldBe` "/oauth2/user+info/profile%20data"
      port req `shouldBe` 443
      host req `shouldBe` "api.example.com"
      queryString req `shouldBe` ""
    it "parse URL with fragments (which should be ignored)" $ do
      req <- liftIO $ uriToRequest [uri|https://api.example.com/callback#access_token=xyz|]
      secure req `shouldBe` True
      path req `shouldBe` "/callback"
      port req `shouldBe` 443
      host req `shouldBe` "api.example.com"
      queryString req `shouldBe` ""
    it "parse URL with non-standard HTTPS port" $ do
      req <- liftIO $ uriToRequest [uri|https://api.example.com:8443/oauth/token|]
      secure req `shouldBe` True
      path req `shouldBe` "/oauth/token"
      port req `shouldBe` 8443
      host req `shouldBe` "api.example.com"
      queryString req `shouldBe` ""
