{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Fitbit where
import           Control.Monad     (mzero)
import           Data.Aeson
import           Data.Text.Lazy    (Text)
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
