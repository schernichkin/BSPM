{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

--import           Control.Monad
--import           Control.Monad.Indexed
import qualified Data.ByteString as BS
import           Data.Int
--import           GHC.TypeLits
import           Control.Monad
import           Lev.Layout      as L
import qualified Lev.Monad       as L
import qualified Lev.Reader      as L

--test_d :: IO (Int32, ByteString)
--test_d = run testLevRead (BS.replicate 16 0)

rGetter :: L.Reader IO ('L.StaticLayout 0 32) Int64
rGetter =
  L.readInt64Host L.>>= \a1 ->
  L.readInt64Host L.>>= \a2 ->
  L.readInt64Host L.>>= \a3 ->
  L.readInt64Host L.>>= \a4 ->
  L.return $ a1 + a2 + a3 + a4
{-# INLINE rGetter #-}

rGetter1 :: L.Reader IO ('L.StaticLayout 4 8) ()
rGetter1 =
  L.printInt64 L.>>= \_ ->
  L.return ()
{-# INLINE rGetter1 #-}

main :: IO ()
main = do
  putStrLn "test"
  L.runReader rGetter1 $ BS.replicate 100 0
  return ()
--  void $ test_d
