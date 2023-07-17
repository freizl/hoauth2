module Network.OAuth2.Experiment.Utils where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import URI.ByteString (URI, serializeURIRef')

tlToBS :: TL.Text -> ByteString
tlToBS = TE.encodeUtf8 . TL.toStrict

bs8ToLazyText :: BS8.ByteString -> TL.Text
bs8ToLazyText = TL.pack . BS8.unpack

unionMapsToQueryParams :: [Map TL.Text TL.Text] -> [(ByteString, ByteString)]
unionMapsToQueryParams =
  map (bimap tlToBS tlToBS)
    . Map.toList
    . Map.unions

uriToText :: URI -> T.Text
uriToText = T.decodeUtf8 . serializeURIRef'
