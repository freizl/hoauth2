{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module IDP.Linkedin where
import           Data.Aeson
import           Data.Text.Lazy    (Text)
import qualified Data.Text.Lazy    as TL
import           GHC.Generics
import           Types
import           URI.ByteString
import           URI.ByteString.QQ

data LinkedinUser = LinkedinUser { firstName :: Text
                                 , lastName  :: Text
                                 } deriving (Show, Generic)

instance FromJSON LinkedinUser where
    parseJSON = genericParseJSON defaultOptions

userInfoUri :: URI
userInfoUri = [uri|https://api.linkedin.com/v1/people/~?format=json|]

toLoginUser :: LinkedinUser -> LoginUser
toLoginUser LinkedinUser {..} = LoginUser { loginUserName = firstName `TL.append` " " `TL.append` lastName }
