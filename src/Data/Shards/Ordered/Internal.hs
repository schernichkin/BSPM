{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Shards.Ordered.Internal
  (
    -- ** Binary search immutable arrays
    binarySearch
  , binarySearchBy
  , binarySearchByBounds
  ) where

import           Data.Bits
import           Data.Key
import           Data.Vector.Algorithms.Search (Comparison)
import           Data.Vector.Generic           (Vector)
import qualified Data.Vector.Generic           as V
import           Data.Void

-- Вводная:
-- 1. Экземпляры Data.Key можно определять для конкретных типов векторов,
--    но не для класса Data.Vector.Generic.Vector потому что класс Vector
--    зависит от 2-х параметров, а все экземпляры Data.Key полиморфны по
--    типу значения.
-- 2. Для создания карты шардов использовать произвольный массив (возможно
--    unboxed, если тип данных шарды это позволяет).
-- 3. Функция поиска может работать с генерализованным вектором.
--    А Lookup - нет, потому что она полиморфная по типу элемента.
--    Может мне просто нужны собственные типы классов?



{-# INLINE binarySearch #-}
binarySearch :: (Vector v e, Ord e)
             => v e
             -> e
             -> Int
binarySearch = binarySearchBy compare

{-# INLINE binarySearchBy #-}
binarySearchBy :: (Vector v e)
               => Comparison e
               -> v e
               -> e
               -> Int
binarySearchBy cmp vec e = binarySearchByBounds cmp vec e 0 (V.length vec)

{-# INLINE binarySearchByBounds #-}
binarySearchByBounds :: (Vector v e)
                     => Comparison e
                     -> v e
                     -> e
                     -> Int
                     -> Int
                     -> Int
binarySearchByBounds cmp vec e = loop
  where
    loop !l !u
      | u <= l    = l
      | otherwise = case V.unsafeIndex vec k `cmp` e of
                      LT -> loop (k + 1) u
                      EQ -> k
                      GT -> loop l k
      where k = (u + l) `shiftR` 1
