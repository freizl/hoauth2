{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Fitbit where
import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

userInfoUri :: URI
userInfoUri = [uri|https://api.fitbit.com/1/user/-/profile.json|]


newtype Fitbit = Fitbit OAuth2 deriving (Show, Generic, Eq)

instance HasLabel Fitbit where
  idpLabel = const "Fitbit"

instance HasTokenReq Fitbit where
  tokenReq (Fitbit key) mgr = fetchAccessToken mgr key

instance HasTokenRefreshReq Fitbit where
  tokenRefreshReq (Fitbit key) mgr = refreshAccessToken mgr key

instance HasUserReq Fitbit where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (toLoginUser re)

instance HasAuthUri Fitbit where
  authUri (Fitbit key) = createCodeUri key [ ("state", "Fitbit.test-state-123")
                                        , ("scope", "profile")
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


toLoginUser :: FitbitUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = userName ouser }
