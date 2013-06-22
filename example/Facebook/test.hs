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
import           Data.Aeson.TH                   (deriveJSON)
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

$(deriveJSON P.id ''User)

--------------------------------------------------

main :: IO ()
main = do
    print $ authorizationUrl facebookKey `appendQueryParam'` facebookScope
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    let (url, body) = accessTokenUrl facebookKey code
    token <- doSimplePostRequest (url, body ++ [("state", "test")])
    userinfo (BL.unpack token) >>= print
    userinfo' (BL.unpack token) >>= print

--------------------------------------------------
-- FaceBook API

-- | Gain read-only access to the user's id, name and email address.
facebookScope :: QueryParams
facebookScope = [("scope", "user_about_me,email")]

-- | Fetch user id and email.
userinfo :: String -> IO BL.ByteString
userinfo accessToken = doSimpleGetRequest (BS.pack ("https://graph.facebook.com/me?fields=id,name,email&" ++ accessToken))

userinfo' :: FromJSON User => String -> IO (Maybe User)
userinfo' accessToken = doJSONGetRequest (BS.pack ("https://graph.facebook.com/me?fields=id,name,email&" ++ accessToken))

