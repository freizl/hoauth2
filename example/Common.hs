{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Data.Char                (chr)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import           Network.Wai              (Request,queryString)
import           Network.HTTP.Types       (Query)


getApiCode :: Request -> B.ByteString
getApiCode request =
    case M.lookup "code" queryMap of
        Just code -> code
        Nothing -> error "request doesn't include code"
  where
    queryMap = convertQueryToMap $ queryString request


convertQueryToMap :: Query -> M.Map B.ByteString B.ByteString
convertQueryToMap query =
    M.fromList $ map normalize query
  where
    normalize (k, Just v) = (k, v)
    normalize (k, Nothing) = (k, B.empty)

lazyBSToString :: BL.ByteString -> String
lazyBSToString s = map (chr . fromIntegral) (BL.unpack s)
