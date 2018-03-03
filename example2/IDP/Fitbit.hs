{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module IDP.Fitbit where
import           Control.Monad                     (mzero)
import           Data.Aeson
import           Data.Bifunctor
import           Data.Text.Lazy                    (Text)
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2
import qualified Network.OAuth.OAuth2.TokenRequest as TR
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Data.Hashable
import Keys
import Utils
import           GHC.Generics


data Fitbit = Fitbit deriving (Show, Generic)

instance Hashable Fitbit

instance IDP Fitbit

instance HasLabel Fitbit

instance HasTokenReq Fitbit where
  tokenReq _ mgr code = fetchAccessToken mgr fitbitKey code

instance HasUserReq Fitbit where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Fitbit where
  authUri _ = createCodeUri fitbitKey [ ("state", "Fitbit.test-state-123")
                                        , ("scope", "user_about_me,email")
                                        ] 

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
getAccessToken = fetchAccessToken
