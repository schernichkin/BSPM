module BSPM.SSSP where

import qualified Data.HashTable.IO as H
import Data.HashTable.IO ( BasicHashTable )
import Control.Concurrent.STM.TMVar
import BSPM.StateStream as SS
import BSPM.Engine.Local
import BSPM.Engine.Local.WorkerSet
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Maybe
import Control.Monad
import Data.Map.Strict as Map
import qualified BSPM.StateStream as SS

type Edge = (Int, Double)
type Graph = Map Int [Edge]

data Estimate = Infinity | From !Int !Double deriving ( Show, Eq )

instance Monoid Estimate where
  mempty = Infinity
  Infinity `mappend` a = a
  a `mappend` Infinity = a
  a@(From _ aw) `mappend` b@(From _ bw) = if (aw <= bw) then a
                                                        else b

data Message = PathToYou Int Double deriving ( Show, Eq )

foldMessages :: Estimate -> BSPMW Message Int s Estimate
foldMessages est = do
  msg <- receive
  -- liftIO $ putStrLn $ "foldMessages: got message " ++ (show msg)
  case msg of
    Just (PathToYou v w) -> foldMessages $ est `mappend` (From v w)
    Nothing -> return est

newVertextWorker :: Graph -> Double -> Int -> IO (Int -> BSPMW Message Int s ())
newVertextWorker graph maxWeight target = do
  distances <- H.new >>= newTMVarIO :: IO (TMVar (BasicHashTable Int Estimate))
  shortestPathWeight <- newIORef maxWeight
  return $ \k -> do
    -- liftIO $ putStrLn $ "Spawning worker for " ++ (show k)
    est <- liftIO $ do
      table <- atomically (takeTMVar distances)
      mbEstimate <- H.lookup table k
      atomically (putTMVar distances table)
      return $ fromMaybe mempty mbEstimate
    -- liftIO $ putStrLn $ "current estimate for " ++ (show k) ++ " " ++ (show est)
    newEst <- foldMessages est
    when (est /= newEst) $ do
      liftIO $ putStrLn $ "new estimate for " ++ (show k) ++ " " ++ (show newEst)
      liftIO $ do
        table <- atomically (takeTMVar distances)
        H.insert table k newEst
        atomically (putTMVar distances table)
      case newEst of
        Infinity -> return () -- should newer happend though
        From _ weight -> do
          spw <- liftIO $ readIORef shortestPathWeight
          when (spw > weight) $ do
            if (k == target)
              then liftIO $ do
                atomicModifyIORef' shortestPathWeight (\w -> (weight, ()))
                putStrLn $ "Found path "
                        ++ " to " ++ (show target) ++ " "
                        ++ (show newEst)
              else do
                let edges = graph Map.! k
                liftIO $ putStrLn $ "Vertext " ++ (show k) ++  " will broadcast new estimate to " ++ (show edges)
                mapM_ (\(v, w) -> sendTo v $ PathToYou k $ w + weight) edges

runOnGraph :: Graph -> Double -> Int -> Int -> IO ()
runOnGraph graph maxWeight from to = do
  liftIO $ putStrLn $ "runOnGraph: maxWeight = " ++ (show maxWeight)
                   ++ " from = " ++ (show from)
                   ++ " to = " ++ (show to)
  worker <- newVertextWorker graph maxWeight to
  runW SS.unit worker $ do
    sendTo from $ PathToYou 1 0
