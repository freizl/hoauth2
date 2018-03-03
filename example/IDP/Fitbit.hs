{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Fitbit where
import           Control.Monad                     (mzero)
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy                    (Text)
import           GHC.Generics
import           Keys
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils


data Fitbit = Fitbit deriving (Show, Generic)

instance Hashable Fitbit

instance IDP Fitbit

instance HasLabel Fitbit

instance HasTokenReq Fitbit where
  tokenReq _ mgr = fetchAccessToken mgr fitbitKey

instance HasUserReq Fitbit where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Fitbit where
  authUri _ = createCodeUri fitbitKey [ ("state", "Fitbit.test-state-123")
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


userInfoUri :: URI
userInfoUri = [uri|https://api.fitbit.com/1/user/-/profile.json|]

toLoginUser :: FitbitUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = userName ouser }
