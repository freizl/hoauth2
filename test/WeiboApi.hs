
module WeiboApi where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

accountUidUri :: BS.ByteString
accountUidUri = pack' "https://api.weibo.com/2/account/get_uid.json"

accountShowUri :: BS.ByteString
accountShowUri = pack' "https://api.weibo.com/2/users/show.json"

pack' = T.encodeUtf8 . T.pack