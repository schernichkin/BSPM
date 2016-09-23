{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

--import           Control.Monad
--import           Control.Monad.Indexed
--import           Data.ByteString       as BS
import           Data.Int
--import           GHC.TypeLits
import           Lev.DoSyntax
import           Lev.Layout   as L
import qualified Lev.Read     as R

--test_d :: IO (Int32, ByteString)
--test_d = run testLevRead (BS.replicate 16 0)

rGetter :: R.Reader IO 0 ('L.StaticLayout 32) Int64
rGetter =
  R.int64Host `R.bindReader` \a1 ->
  R.int64Host `R.bindReader` \a2 ->
  R.int64Host `R.bindReader` \a3 ->
  R.int64Host `R.bindReader` \a4 ->
  R.returnReader $ a1 + a2 + a3 + a4
{-# INLINE rGetter #-}


main :: IO ()
main = do
  return ()
--  void $ test_d
