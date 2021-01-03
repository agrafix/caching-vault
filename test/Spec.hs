import Test.Hspec

import Data.Time
import Data.Time.TimeSpan

import Control.Monad (replicateM)
import Data.List (nub)
import qualified Data.Cache.Vault as C

main :: IO ()
main =
  hspec $
  do it "allows reading previously written values" $
       do now <- getCurrentTime
          cache <- C.newCache
          let key = C.mintLabeledKey "foo"
          C.insert key Nothing False cache
          C.lookup now key cache `shouldReturn` Just False
     it "expires values" $
       do now <- getCurrentTime
          cache <- C.newCache
          let key = C.mintLabeledKey "foo"
          C.insert key (Just now) False cache
          C.lookup (addUTCTimeTS (minutes 1) now) key cache `shouldReturn` Nothing
     it "deletes values" $
       do now <- getCurrentTime
          cache <- C.newCache
          let key = C.mintLabeledKey "foo"
          C.insert key Nothing False cache
          C.delete key cache
          C.lookup now key cache `shouldReturn` Nothing
     it "purges values" $
       do now <- getCurrentTime
          cache <- C.newCache
          let key = C.mintLabeledKey "foo"
          C.insert key Nothing False cache
          C.reset cache
          C.lookup now key cache `shouldReturn` Nothing
     it "generates distinct unique keys" $
       do keys <- replicateM 200 C.mintUniqKey
          keys `shouldBe` nub keys
     it "differentiates labeled and unique keys" $
       do let key :: C.Key Bool
              key = C.mintLabeledKey "foo"
          uniqKey <- C.mintUniqKey
          key `shouldNotBe` uniqKey
     it "keys with same label of different types are different" $
       do now <- getCurrentTime
          cache <- C.newCache
          let key1 :: C.Key Bool
              key1 = C.mintLabeledKey "foo"

              key2 :: C.Key Int
              key2 = C.mintLabeledKey "foo"
          C.insert key1 Nothing False cache
          C.lookup now key1 cache `shouldReturn` Just False
          C.lookup now key2 cache `shouldReturn` Nothing
