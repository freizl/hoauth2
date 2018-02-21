{-# LANGUAGE OverloadedStrings #-}

module TokenUtil where

import           Data.ByteString                   (ByteString)
import qualified Data.Text.Encoding                as TE
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Types
import           URI.ByteString

getAT, postAT:: Manager
  -> OAuth2
  -> ExchangeToken
  -> IO (OAuth2Result TR.Errors OAuth2Token)
getAT = fetchAccessToken
postAT = postATX doJSONPostRequest

postATX :: (Manager -> OAuth2 -> URI -> PostBody -> IO (OAuth2Result TR.Errors OAuth2Token))
        -> Manager
        -> OAuth2
        -> ExchangeToken
        -> IO (OAuth2Result TR.Errors OAuth2Token)
postATX postFn mgr okey code = do
  let (url, body1) = accessTokenUrl okey code
  let extraBody = authClientBody okey
  postFn mgr okey url (extraBody ++ body1)

authClientBody :: OAuth2 -> [(ByteString, ByteString)]
authClientBody okey = [ ("client_id", TE.encodeUtf8 $ oauthClientId okey)
                      , ("client_secret", TE.encodeUtf8 $ oauthClientSecret okey)
                      ]
