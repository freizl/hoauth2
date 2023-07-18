module Utils where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Web.Scotty.Internal.Types

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

parseValue :: Aeson.FromJSON a => Maybe Aeson.Value -> Maybe a
parseValue Nothing = Nothing
parseValue (Just a) = case Aeson.fromJSON a of
  Aeson.Error _ -> Nothing
  Aeson.Success b -> Just b
