{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Lev.Layout where

import           Data.Int
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TH
import           Data.Singletons.TypeLits

#include "MachDeps.h"

type family SizeOf a :: Nat
type instance SizeOf Int16 = SIZEOF_INT16
type instance SizeOf Int32 = SIZEOF_INT32
type instance SizeOf Int64 = SIZEOF_INT64

type Size   = Nat
type Offset = Nat

data Layout = DynamicLayout | StaticLayout Size

data instance Sing (a :: Layout) where
  SDynamicLayout :: Sing 'DynamicLayout
  SStaticLayout  :: Sing size -> Sing ('StaticLayout size)

instance (SingI size) => SingI ('StaticLayout size) where
  sing = SStaticLayout sing

instance SingI 'DynamicLayout where
  sing = SDynamicLayout

type family SequentialLayout (a :: Layout) (b :: Layout) :: Layout where
  SequentialLayout ('StaticLayout s1) ('StaticLayout s2) = 'StaticLayout (s1 :+ s2)
  SequentialLayout _ _ = 'DynamicLayout

type family SequentialOffset (o :: Offset) (l :: Layout) :: Offset where
  SequentialOffset o ('StaticLayout s) = o :+ s
  SequentialOffset o 'DynamicLayout = o
