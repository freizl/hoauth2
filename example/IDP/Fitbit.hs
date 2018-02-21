{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Fitbit where
import           Control.Monad                     (mzero)
import           Data.Aeson
import           Data.Bifunctor
import           Data.Text.Lazy                    (Text)
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           TokenUtil
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

data FitbitUser = FitbitUser
    { userId   :: Text
    , userName :: Text
    , userAge  :: Int
    } deriving (Show, Eq)

instance FromJSON FitbitUser where
    parseJSON (Object o) =
        FitbitUser
        <$> ((o .: "user") >>= (.: "encodedId"))
        <*> ((o .: "user") >>= (.: "fullName"))
        <*> ((o .: "user") >>= (.: "age"))
    parseJSON _ = mzero


userInfoUri :: URI
userInfoUri = [uri|https://api.fitbit.com/1/user/-/profile.json|]

toLoginUser :: FitbitUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = userName ouser }

getUserInfo :: FromJSON a => Manager -> AccessToken -> IO (OAuth2Result a LoginUser)
getUserInfo mgr at = do
  re <- authGetJSON mgr at userInfoUri
  return (second toLoginUser re)

getAccessToken :: Manager
               -> OAuth2
               -> ExchangeToken
               -> IO (OAuth2Result TR.Errors OAuth2Token)
getAccessToken = getAT
