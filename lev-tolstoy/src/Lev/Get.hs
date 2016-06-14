{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lev.Get
  ( Get
  , FixedGetter
  , run
  , get
  , prim
  , int16Host
  , int32Host
  , int64Host
  ) where

import           Control.Arrow
import           Control.Category
import           Control.Monad.Primitive
import           Data.Int
import           Data.Primitive
import           Data.Profunctor
import           Data.Word
import           Foreign.ForeignPtr
import           GHC.Ptr
import           Lev.Buffer
import           Prelude                 hiding (id, (.))

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Get." ++ fun ++ ':':' ':msg

{-# NOINLINE moduleError #-}
moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)

{-# INLINE checkBufferLength #-}
checkBufferLength :: Int -> Int -> a -> a
checkBufferLength required remains r =
  if required <= remains
    then r
    else moduleError "checkBufferLength"
                   ( "Bytes remains = " ++ (show remains) ++
                     ", bytes required = " ++ (show required) )

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

run :: Get () b -> Buffer -> (b, Buffer)
run (GetArr f) b = (f (), b)
run (GetFixed FixedGetter {..}) (Buffer {..}) =
    checkBufferLength _fixedSize _length
  $ unsafeInlineIO
  $ withForeignPtr _base $ \(Ptr addr) -> return
    ( _fixedGetter _base () ((Addr addr) `plusAddr` _offset)
    , Buffer _base (_offset + _fixedSize) (_length - _fixedSize)
    )
