-- | Resolves name conflicts and provides convenient lexical scope for using
-- graph tools from ghci

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Graph.Interactive
  ( readShumov
  , unconsShumov
  , otop
  , otopBy
  , osample
  , module  X
  ) where

import           Data.Graph.Shumov    (Shumov, ShumovVertex)
import qualified Data.Graph.Shumov    as SH
import           Data.MonoTraversable as X
import Control.Monad
import qualified Data.Vector.Generic as GV
import  Data.Vector.Generic (Vector )
import  Data.Vector.Generic.Mutable (MVector )
import qualified Data.Vector.Generic.Mutable as GMV
import           Data.Vector.Algorithms.Heap as VA
import           Control.Monad.Trans.Class
import  qualified Data.Vector as V
import System.Random.MWC
import Data.Function

-- TODO: возможно имеет смысл использовать тайпклассы, чтобы уменьшить кол-во
-- имён (правда тогда придётся указывать типы)

readShumov :: FilePath -> IO Shumov
readShumov = SH.readFile

unconsShumov :: Shumov -> Maybe (ShumovVertex, Shumov)
unconsShumov = SH.uncons

otop :: ( MonoFoldable mono
        , Vector v (Element mono)
        , Ord (Element mono)
        )
     => Int -> mono -> v (Element mono)
otop = otopBy compare

otopBy :: ( MonoFoldable mono
          , Vector v (Element mono)
          )
       => (Element mono -> Element mono -> Ordering)
       -> Int
       -> mono
       -> v (Element mono)
otopBy _   0    _ = GV.empty
otopBy cmp n mono = GV.create $ do
  h <- GMV.new n
  let f u e | u == n = do
               topElem <- GMV.unsafeRead h 0
               when (cmp e topElem == LT) $ do
                 VA.pop cmp h 0 (n - 1)
                 heapInsert cmp h 0 (n - 1) e
               return n
            | otherwise = do
               GMV.unsafeWrite h u e
               if (u + 1 == n)
                 then do
                   heapify cmp h 0 n
                   return n
                 else return $ u + 1
  u <- ofoldlM f 0 mono
  when (u < n) $ heapify cmp h 0 u
  sortHeap cmp h 0 0 u
  return $ GMV.unsafeTake u h

osample :: ( MonoFoldable mono
           , Vector v (Element mono)
           , Vector v (Int, Element mono)
           , Ord (Element mono)
           )
        => Int -> mono -> IO (v (Element mono))
osample n mono = withSystemRandom . asGenST $ \gen -> do
  xs <- forM (otoList mono) $ \e -> do
    (r::Int) <- uniform gen
    return (r, e)
  return $ GV.map snd $ otopBy (compare `on` fst) n xs
