{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module BSP
    ( Process
    , Step
--      run
--    , read -- TODO: change names to not conflict with prelude
    , step
--    , peerId
--    , thisId
--    , write
    ) where

import           BSP.Util.CountDown
import           BSP.Util.InitOnce
import           Control.Category
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import           Prelude                    hiding (read, (.))

-- Предположения для структуры данных для шага.
-- 1. Будет производиться много записей в shared state пира с разрешением по id,
--    операция записи должна производится максимально быстро.
-- 2. Будет мало шагов и мало пиров внутри шага, скорость перехода от шага к шагу
--    не критична.

-- Исходя из этого:
-- 1. Можно строить специализированные структуры данных для выполнения шага.
-- 2. Комбинация двух шагов шагом не является, построение для неё специализированной
--    структуры смысла не имеет.
-- 3. Переиспользовать нитки или вызывать forkIO на каждом шаге - не критично.

-- Проблемы:
-- 1. Нужно уметь восстанавливать исходную структуру из массива.
--    Решение:
--      Для этого можно использовать исходную структуру при условии что она Traversable.
-- 2. Использовать fmap и zip (zip может быть неестественным для шардов)
-- 3. Чистый fmap. Единственная проблема с восстановлением структуры в том, что её
--    ноды имеют рекурсивные ссылки на саму структуру. Это решается через fix.
--    _peers можно вообще вынести за пределы структуры и фмапнуть отдельно.

data PeerState t i o = PeerState
  { _currentState :: !i
  , _sharedState  :: !(t (IORef o))
  }

newtype Step t i o r = Step { unStep :: ReaderT (PeerState t i o) IO r }
    deriving (Functor, Applicative, Monad, MonadIO)

runStep :: ( Traversable t, Monoid o ) => (a -> Step t i o r) -> t (i, a) -> IO (t (o, r))
runStep step t = do
  counter <- newCountDown $ length t
  mfix $ \s -> forM t $ \(i, a) -> do
    let peerState = PeerState i s
    runReaderT (unStep $ step a) peerState
    newIORef mempty

  return undefined

type family Fst a where Fst (a, b) = a
type family Snd a where Snd (a, b) = b

-- IO нужна для создания примитивов синхронизации, используемых процессом.
data Process (t :: * -> *) a b = Process { unProcess :: IO (Snd a -> Step t (Fst a) (Fst b) (Snd b)) }

step :: (a -> Step t i o r) -> Process t (i, a) (o, r)
step = Process . return

data SyncPoint t a i o r = SyncPoint
  { _firstStep :: a -> Step t i o r
  }

instance Category (Process t) where
  id = Process $ return return

  f . g = Process $ do
    syncPointOnce <- newInitOnce
    return $ \a -> do
      syncPoint <- liftIO $ getInitOnce syncPointOnce $ do
        firstStep <- unProcess g
        return SyncPoint
          { _firstStep = firstStep
          }
      let process = _firstStep syncPoint a
      undefined
