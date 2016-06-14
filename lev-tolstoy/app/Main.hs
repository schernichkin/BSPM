{-# LANGUAGE Arrows       #-}

module Main where

import           Control.Arrow
import           Data.Int
import           Lev.Get

test_a :: Get a Int32
test_a = (get int32Host)
   >>> (get int32Host)
   >>> (get int32Host)
   >>> (get int32Host)

test_b :: Get a (Int32, Int32, Int32, Int32)
test_b = proc _ -> do
  a <- (get int32Host) -< ()
  b <- (get int32Host) -< ()
  c <- (get int32Host) -< ()
  d <- (get int32Host) -< ()
  returnA -< (a, b, c, d)


  (\ @ a
     (w :: GHC.ForeignPtr.ForeignPtr GHC.Word.Word8)
     (w1 :: a)
     (w2 :: Data.Primitive.Types.Addr) ->
   let {
     addr :: Data.Primitive.Types.Addr
     = case w2 of wild2 { Data.Primitive.Types.Addr a# ->
       Data.Primitive.Types.Addr (GHC.Prim.plusAddr# a# 4#) }
   } in
   let {
     addr1 :: Data.Primitive.Types.Addr
     = case addr of wild2 { Data.Primitive.Types.Addr a# ->
       Data.Primitive.Types.Addr (GHC.Prim.plusAddr# a# 4#) }
   } in
   (case w2 of wild { Data.Primitive.Types.Addr addr# ->
    case GHC.Prim.indexInt32OffAddr# addr# 0# of wild1 { DEFAULT ->
    GHC.Int.I32# wild1 } },
    case addr of wild { Data.Primitive.Types.Addr addr# ->
    case GHC.Prim.indexInt32OffAddr# addr# 0# of wild1 { DEFAULT ->
    GHC.Int.I32# wild1 } },
    case addr1 of wild { Data.Primitive.Types.Addr addr# ->
    case GHC.Prim.indexInt32OffAddr# addr# 0# of wild1 { DEFAULT ->
    GHC.Int.I32# wild1 } },
    case addr1 of wild2 { Data.Primitive.Types.Addr a# ->
    case GHC.Prim.indexInt32OffAddr#
           (GHC.Prim.plusAddr# a# 4#)
           0# of wild { DEFAULT ->
    GHC.Int.I32# wild } }))

main :: IO ()
main = do
  return ()
