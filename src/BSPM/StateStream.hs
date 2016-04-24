module BSPM.StateStream
  (  StateStream (..)
  , const
  , new
  , unit
  ) where

import Prelude hiding ( const )

import Control.Comonad.Cofree

type StateStream a = Cofree IO a

const :: s -> StateStream s
const = coiter return

unit :: StateStream ()
unit = const ()

new :: IO a -> IO (StateStream a)
new initializer = do
  a <- initializer
  return $ a :< new initializer
