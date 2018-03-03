module Utils where

import qualified Data.Aeson                as Aeson
import           Data.ByteString           (ByteString)
import qualified Data.Text.Encoding        as TE
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as TL
import           Network.OAuth.OAuth2
import           URI.ByteString
import           Web.Scotty.Internal.Types

tlToBS :: TL.Text -> ByteString
tlToBS = TE.encodeUtf8 . TL.toStrict

paramValue :: Text -> [Param] -> [Text]
paramValue key = fmap snd . filter (hasParam key)

hasParam :: Text -> Param -> Bool
hasParam t = (== t) . fst

parseValue :: Aeson.FromJSON a => Maybe Aeson.Value -> Maybe a
parseValue Nothing = Nothing
parseValue (Just a) = case Aeson.fromJSON a of
  Aeson.Error _   -> Nothing
  Aeson.Success b -> Just b

createCodeUri :: OAuth2
  -> [(ByteString, ByteString)]
  -> Text
createCodeUri key params = TL.fromStrict $ TE.decodeUtf8 $ serializeURIRef'
  $ appendQueryParams params
  $ authorizationUrl key


