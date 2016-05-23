module Spec where

import           Data.Key
import           Data.Shards.Ordered.Internal
import qualified Data.Vector                          as V
import           Data.Vector.Utils
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck

vectorUtilsTest :: Test
vectorUtilsTest = testGroup "Data.Vector.Utils"
  [ testGroup "salami"
    [ testCase "empty" $
        let result = salami 10 V.empty :: [V.Vector Int]
        in result @?= []
    , testCase "single" $
        let result = salami 1 $ V.fromList [1, 2, 3] :: [V.Vector Int]
        in result @?= [V.fromList [1, 2, 3]]
    , testCase "elements" $
        let result = salami 10 $ V.fromList [1, 2, 3] :: [V.Vector Int]
        in result @?= [V.singleton 1, V.singleton 2, V.singleton 3]
    , testCase "slices" $
        let result = salami 3 $ V.fromList [1, 2, 3, 4, 5, 6, 7] :: [V.Vector Int]
        in result @?= [V.fromList [1, 2], V.fromList [3, 4], V.fromList [5, 6, 7]]
    ]
  ]

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
  [ vectorUtilsTest
  , shardsTest
  ]
