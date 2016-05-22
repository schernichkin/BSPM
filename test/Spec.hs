module Spec where

import           Data.Key
import           Data.Shards.Ordered.Internal
import qualified Data.Vector                          as V
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck

shardsTest :: Test
shardsTest = testGroup "Shards"
  [ testGroup "index" $
      let shardMap = OrderedShardMap $ V.fromList
                   [ OrderedShardEntry 1 1
                   , OrderedShardEntry 2 2
                   , OrderedShardEntry 3 3
                   , OrderedShardEntry 5 5
                   , OrderedShardEntry 6 6 ] :: OrderedShardMap Int Int in
      [ testCase "lower value out of bounds" $
          index shardMap 0 @?= 1
      , testCase "lower value" $
          index shardMap 1 @?= 1
      , testCase "inner value exists" $
          index shardMap 3 @?= 3
      , testCase "inner value not exists" $
          index shardMap 4 @?= 5
      , testCase "upper value" $
          index shardMap  6 @?= 6
      , testCase "upper value out of bounds" $
          index shardMap  7 @?= 6
      ]
  ]

main :: IO ()
main = defaultMain
  [  shardsTest
  ]
