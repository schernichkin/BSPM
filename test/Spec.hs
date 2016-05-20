module Spec where

import           Data.Shards.Ordered.Internal
import qualified Data.Vector                          as V
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck

shardsTest :: Test
shardsTest = testGroup "Shards"
  [ testGroup "binarySearch" $
      let vector = V.fromList [1, 2, 3, 5, 6] in
      [ testCase "lower value out of bounds" $
          binarySearch vector 0 @?= 0
      , testCase "lower value" $
          binarySearch vector 1 @?= 0
      , testCase "inner value exists" $
          binarySearch vector 3 @?= 2
      , testCase "inner value not exists" $
          binarySearch vector 4 @?= 3
      , testCase "upper value" $
          binarySearch vector 6 @?= 4
      , testCase "upper value out of bounds" $
          binarySearch vector 7 @?= 5
      ]
  ]

main :: IO ()
main = defaultMain
  [  shardsTest
  ]
