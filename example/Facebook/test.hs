{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

{- Facebook example -}

module Main where

import           Keys                       (facebookKey)
import           Network.OAuth.OAuth2

import           Data.Aeson                 (FromJSON)
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Network.HTTP.Conduit
import           Prelude                    hiding (id)
import           URI.ByteString
import           URI.ByteString.QQ

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }


--------------------------------------------------

data User = User { id    :: Text
                 , name  :: Text
                 , email :: Text
                 } deriving (Show)

$(deriveJSON defaultOptions ''User)

--------------------------------------------------

main :: IO ()
main = do
    print $ serializeURIRef' $ appendQueryParams  facebookScope $ authorizationUrl facebookKey
    putStrLn "visit the url and paste code here: "
    code <- getLine
    mgr <- newManager tlsManagerSettings
    let (url, body) = accessTokenUrl facebookKey $ ExchangeToken $ T.pack code
    let extraBody = [ ("state", "test")
                    , ("client_id", T.encodeUtf8 $ oauthClientId facebookKey)
                    , ("client_secret", T.encodeUtf8 $ oauthClientSecret facebookKey)
                    ]
    resp <- doJSONPostRequest mgr facebookKey url (body ++ extraBody)
    case (resp :: OAuth2Result Errors OAuth2Token) of
      Right token -> do
                     print token
                     userinfo mgr (accessToken token) >>= print
                     userinfo' mgr (accessToken token) >>= print
      Left l -> print l

--------------------------------------------------
-- FaceBook API

-- | Gain read-only access to the user's id, name and email address.
facebookScope :: QueryParams
facebookScope = [("scope", "user_about_me,email")]

-- | Fetch user id and email.
userinfo :: Manager -> AccessToken -> IO (OAuth2Result Errors BL.ByteString)
userinfo mgr token = authGetBS mgr token    [uri|https://graph.facebook.com/me?fields=id,name,email|]

userinfo' :: FromJSON User => Manager -> AccessToken -> IO (OAuth2Result Errors User)
userinfo' mgr token = authGetJSON mgr token [uri|https://graph.facebook.com/me?fields=id,name,email|]
