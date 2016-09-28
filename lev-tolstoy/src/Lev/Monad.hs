{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Lev.Monad
  ( LevMonad (..)
  , module X
  ) where

import           Lev.Layout as X
import           Lev.Reader as X
import           Prelude ( Monad(), String, error )
import           Data.Singletons

class LevMonad (m :: Layout -> * -> *) where
  (>>=)  :: (SingI f, SingI g) => (BindLayoutInv f g) => m f a -> (a -> m g b) -> m (BindLayout f g) b

  return :: a -> m (UnitLayout o) a

  {-# INLINE fail #-}
  fail   :: String -> m l a
  fail = error

instance (Monad m) => LevMonad (Reader m) where
  {-# INLINE (>>=) #-}
  (>>=) = bindReader

  {-# INLINE return #-}
  return = unitReader
