{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Weibo where
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Hashable
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           GHC.Generics
import           Keys
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

data Weibo = Weibo deriving (Show, Generic)

instance Hashable Weibo

instance IDP Weibo

instance HasLabel Weibo

instance HasTokenReq Weibo where
  tokenReq _ mgr = fetchAccessToken mgr weiboKey

-- fetch user info via
-- GET
-- access token in query param only
instance HasUserReq Weibo where
  userReq _ mgr at = do
    re <- authGetBS2 mgr at userInfoUri
    return (re >>= (bimap BSL.pack toLoginUser . eitherDecode))

instance HasAuthUri Weibo where
  authUri _ = createCodeUri weiboKey [ ("state", "Weibo.test-state-123")
                                        ]

-- TODO: http://open.weibo.com/wiki/2/users/show
data WeiboUser = WeiboUser { id         :: Integer
                           , name       :: Text
                           , screenName :: Text
                           } deriving (Show, Generic)

newtype WeiboUID = WeiboUID { uid :: Integer }
  deriving (Show, Generic)

instance FromJSON WeiboUID where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON WeiboUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.weibo.com/2/account/get_uid.json|]

toLoginUser :: WeiboUID -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = TL.pack $ show $ uid ouser }
