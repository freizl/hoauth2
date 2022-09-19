module Utils where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Web.Scotty.Internal.Types

bslToText :: BSL.ByteString -> Text
bslToText = TL.pack . BSL.unpack

paramValue :: Text -> [Param] -> [Text]
paramValue key = fmap snd . filter (hasParam key)

hasParam :: Text -> Param -> Bool
hasParam t = (== t) . fst

parseValue :: Aeson.FromJSON a => Maybe Aeson.Value -> Maybe a
parseValue Nothing = Nothing
parseValue (Just a) = case Aeson.fromJSON a of
  Aeson.Error _ -> Nothing
  Aeson.Success b -> Just b
