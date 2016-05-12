module BSP
    (
    ) where

import Control.Monad.Indexed

data Process a b r = Process

instance IxFunctor Process where
  imap f = const Process

instance IxPointed Process where
  ireturn = const Process

instance IxApplicative Process where
  iap a = const Process

instance IxMonad Process where
  ibind m = const Process
