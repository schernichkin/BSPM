{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lev.Get
  ( Get
  , FixedGetter
  , prim
  , int16Host
  , int32Host
  , int64Host
  , get
  ) where

import           Control.Arrow
import           Control.Category
import           Data.Int
import           Data.Primitive
import           Data.Profunctor
import           Data.Word
import           Foreign.ForeignPtr
import           Prelude            hiding (id, (.))

data FixedGetter a b = FixedGetter
  { _fixedSize   :: !Int
  -- | Указатели или смещения?
  --   - Смещения могут быть вычислены статически, сейчас конструкции вроде
  --     a >>> b >>> c не генерируют операции чтения памяти для a и b
  --     но генерируют операции plusAddr. С другой стороны такие парсеры вряд
  --     ли будут встречаться в реальных программах.
  --   - При использовании смещений для первого адреса будет вызываться
  --     plusAddr, сейчас она не вызывается. Но, учитывая, что мы работаем
  --     с ByteString, смещение у нас всегда будет в любом случае.
  --   - Использование указателей генерируюет let-биндинги (по крайней мере для
  --     boxed типов), нужно понять, во что они компилируются. 
  , _fixedGetter :: !(ForeignPtr Word8 -> a -> Addr -> b)
  }

prim :: forall a b . (Prim b) => FixedGetter a b
prim = FixedGetter
  { _fixedSize = sizeOf (undefined :: b)
  , _fixedGetter = const . const $ flip indexOffAddr 0
  }
{-# INLINE prim #-}

int16Host :: FixedGetter a Int16
int16Host = prim
{-# INLINE int16Host #-}

int32Host :: FixedGetter a Int32
int32Host = prim
{-# INLINE int32Host #-}

int64Host :: FixedGetter a Int64
int64Host = prim
{-# INLINE int64Host #-}

data Get a b = GetArr   !(a -> b)
             | GetFixed !(FixedGetter a b)

instance Profunctor Get where
  dimap f g (GetArr a) = GetArr $ g . a . f

  dimap f g (GetFixed FixedGetter{..}) = GetFixed $ FixedGetter
    { _fixedSize   = _fixedSize
    , _fixedGetter = \base a addr -> g $ _fixedGetter base (f a) addr
    }

instance Category Get where
  id = GetArr id

  GetArr f . GetArr g = GetArr $ f . g
  GetArr f . g = rmap f g
  f . GetArr g = lmap g f

  (GetFixed fix1) . (GetFixed fix2) = GetFixed $ FixedGetter
    { _fixedSize   = _fixedSize fix1 + _fixedSize fix2
    , _fixedGetter = \base a addr ->
      -- это работает неправильно. адрес надо передавать через стрелку.
      let b = _fixedGetter fix2 base a addr
      in _fixedGetter fix1 base b (addr `plusAddr` _fixedSize fix2)
    }

instance Arrow Get where
  arr = GetArr
  first (GetArr f) = GetArr $ \(a, b) -> (f a, b)
  first (GetFixed FixedGetter{..}) = GetFixed $ FixedGetter
    { _fixedSize   = _fixedSize
    , _fixedGetter = \base (a, b) addr -> ( _fixedGetter base a addr, b )
    }

get :: FixedGetter a b -> Get a b
get = GetFixed
{-# INLINE CONLIKE get #-}
