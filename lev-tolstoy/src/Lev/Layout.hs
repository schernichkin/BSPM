{-# LANGUAGE CPP           #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Lev.Layout where

import           Data.Int
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TypeLits
import           Data.Word
import           GHC.Exts

#include "MachDeps.h"

type family SizeOf a :: Nat
type instance SizeOf Word8 = 1
type instance SizeOf Int16 = SIZEOF_INT16
type instance SizeOf Int32 = SIZEOF_INT32
type instance SizeOf Int64 = SIZEOF_INT64

type Size   = Nat
type Offset = Nat

data Layout = DynamicLayout | StaticLayout Offset Size

data instance Sing (a :: Layout) where
  SDynamicLayout :: Sing 'DynamicLayout
  SStaticLayout  :: Sing offset -> Sing size -> Sing ('StaticLayout offset size)

instance (SingI offset, SingI size) => SingI ('StaticLayout offset size) where
  sing = SStaticLayout sing sing

instance SingI 'DynamicLayout where
  sing = SDynamicLayout

type UnitLayout o = 'StaticLayout o 0

type family BindLayout (a :: Layout) (b :: Layout) :: Layout where
  BindLayout ('StaticLayout o1 s1) ('StaticLayout o2 s2) = 'StaticLayout o1 (s1 :+ s2)
  BindLayout _ _ = 'DynamicLayout

type family BindLayoutInv (a :: Layout) (b :: Layout) :: Constraint where
  BindLayoutInv ('StaticLayout o1 s1) ('StaticLayout o2 s2) = ((o1 :+ s1) ~ o2)
  BindLayoutInv _ _ = ()
