{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- Facebook example -}

module Main where

import           Keys                            (facebookKey)
import           Network.OAuth.OAuth2
import           Network.OAuth.OAuth2.HttpClient

import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad                   (mzero)
import           Data.Aeson                      (FromJSON, Value (Object),
                                                  parseJSON, (.:), (.:?))
import           Data.Aeson.TH                   (deriveJSON, defaultOptions)
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as BL
import           Data.Text                       (Text)
import           Prelude                         hiding (id)
import qualified Prelude                         as P (id)
import           System.Environment              (getArgs)

--------------------------------------------------

data User = User { id    :: Text
                 , name  :: Text
                 , email :: Text
                 } deriving (Show)

$(deriveJSON defaultOptions ''User)

--------------------------------------------------

main :: IO ()
main = do
    print $ authorizationUrl facebookKey `appendQueryParam` facebookScope
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    let (url, body) = accessTokenUrl facebookKey code
    (Right token) <- doJSONPostRequest url (body ++ [("state", "test")])
    userinfo token >>= print
    userinfo' token >>= print

--------------------------------------------------
-- FaceBook API

-- | Gain read-only access to the user's id, name and email address.
facebookScope :: QueryParams
facebookScope = [("scope", "user_about_me,email")]

-- | Fetch user id and email.
userinfo :: AccessToken -> IO (OAuth2Result BL.ByteString)
userinfo token = authGetBS token "https://graph.facebook.com/me?fields=id,name,email&"

userinfo' :: FromJSON User => AccessToken -> IO (OAuth2Result User)
userinfo' token = authGetJSON token "https://graph.facebook.com/me?fields=id,name,email"

