module Utils where

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Web.Scotty ( Param )

bslToText :: BSL.ByteString -> Text
bslToText = TL.pack . BSL.unpack

paramValue :: Text -> [Param] -> [Text]
paramValue key = fmap snd . filter (hasParam key)

paramValueMaybe :: Text -> [Param] -> Maybe Text
paramValueMaybe key xs = case filter (hasParam key) xs of
  [a] -> Just (snd a)
  _ -> Nothing

hasParam :: Text -> Param -> Bool
hasParam t = (== t) . fst

