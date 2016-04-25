module BSPM.SSSP where

import Data.HashTable.IO as H
import Control.Concurrent.STM.TMVar
import BSPM.StateStream as SS
import BSPM.Engine.Local
import BSPM.Engine.Local.WorkerSet
import Data.Vector
import Data.IORef

data Edge = Edge !Int ![(Int, Double)]

data Estimate = Estimate !Double !Int

data Message = ShortestPath Int
             | PathToYou Int Double
             | BackTrace [Int]

{-
-- Для обновления критерия отсечки нужно либо хранить его в глобальном состоянии,
-- либо добавить возможность широковещательй рассылки всем рабочим воркерам.
-- Сообщения широковещательной рассылки должны посылаться после того, как отработают
-- все воркеры, т.к. в процессе работы воркер может создавать новые воркеры.
-- Сейчас широковещательная рассылка используется для отправки сообщений завершения
-- текущего шага, можно будет расширить этот механизм. Для но локального движка лучше
-- пока использовать локальное разделяемое состояние.
-}

newVertextWorker :: IO (k -> BSPMW a k s ())
newVertextWorker = do
  distances <- H.new >>= newTMVarIO :: IO (TMVar (BasicHashTable Int Estimate))
  shortestPath <- new
  return $ \v -> do
    return ()
{-
newSateStream :: IO (StateStream SharedState)
newSateStream = do
  distances <- H.new >>= newTMVarIO
  return $ SS.const SharedState
    { _distances = distances
    }
-}
