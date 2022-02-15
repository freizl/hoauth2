{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

{-
  disabled since it's not yet working. error:
  - serviceErrorCode:100
  - message:Not enough permissions to access /me GET
-}
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
                                 } deriving (Show, Generic, Eq)

instance FromJSON LinkedinUser where
    parseJSON = genericParseJSON defaultOptions

userInfoUri :: URI
userInfoUri = [uri|https://api.linkedin.com/v2/me|]


toLoginUser :: LinkedinUser -> LoginUser
toLoginUser LinkedinUser {..} = LoginUser { loginUserName = firstName `TL.append` " " `TL.append` lastName }

{-
mkIDPData Linkedin =
  let userUri = createCodeUri linkedinKey [("state", "linkedin.test-state-123")]
  in
  IDPData { codeFlowUri = userUri
          , loginUser = Nothing
          , idpName = Linkedin
          , oauth2Key = linkedinKey
          , toFetchAccessToken = postAT
          , userApiUri = ILinkedin.userInfoUri
          , toLoginUser = ILinkedin.toLoginUser
          }
-}
