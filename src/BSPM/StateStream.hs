module BSPM.StateStream
  (  StateStream (..)
  , const
  , unit
  ) where

import Prelude hiding ( const )
import Control.Comonad.Cofree

type StateStream a = Cofree IO a

const :: s -> StateStream s
const = coiter return

unit :: StateStream ()
unit = const ()
