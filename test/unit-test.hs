{-# LANGUAGE OverloadedStrings #-}



import qualified Data.ByteString.Lazy as BSL
import Control.Applicative ((<$>))
import Data.Aeson (decode)
import Test.HUnit

import Network.OAuth2.OAuth2
import Network.OAuth2.Utils

-- FIXME: unit test

main :: IO ()
main = do
	   print prop_getUid
	   print prop_getInvalidUid
	   print $ intToByteString 1234

invalidUidString :: BSL.ByteString
invalidUidString = "{\"uid\" : \"222222\" }"

prop_getInvalidUid :: Maybe WeiboUserId
prop_getInvalidUid = decode invalidUidString

uidString :: BSL.ByteString
uidString = "{\"uid\" : 222222 }"

prop_getUid :: Maybe WeiboUserId
prop_getUid = decode uidString


--tests = TestList [TestLabel "test1" test1]
--test1 = TestCase (assertEqual "for invalid uid response," Nothing prop_getInvalidUid)

