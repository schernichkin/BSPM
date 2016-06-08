{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module BSPM.SSSP where

{-
import qualified Data.HashTable.IO as H
import Data.HashTable.IO ( BasicHashTable )
import Control.Concurrent.STM.TMVar
import BSPM.Engine.Local
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Maybe
import Control.Monad
import Data.Graph.Class
import Data.MonoTraversable

sssp :: ( Graph g )
     => g
     -> Vertex g
     -> Vertex g
     -> (Vertex g -> IO Bool)
     -> (Edge g -> IO Bool)
     -> Int
     -> Double
     -> IO ()
     -> IO ()
sssp graph source target vertexFilter edgeFilter maxHops maxWeight pathFound = do

  return undefined


data Estimate = Infinity | From !Int !Double deriving ( Show, Eq )

instance Monoid Estimate where
  mempty = Infinity
  Infinity `mappend` a = a
  a `mappend` Infinity = a
  a@(From _ aw) `mappend` b@(From _ bw) = if (aw <= bw) then a
                                                        else b
data Message = PathToYou Int Double deriving ( Show, Eq )

foldMessages :: Estimate -> BSPM Message Int Estimate
foldMessages est = do
  msg <- receive
  -- liftIO $ putStrLn $ "foldMessages: got message " ++ (show msg)
  case msg of
    Just (PathToYou v w) -> foldMessages $ est `mappend` (From v w)
    Nothing -> return est

newVertextWorker :: ( Graph g
                    , Vertex g ~ Int
                    , Edge g ~ (Int, Double)
                    , MonoFoldable (Edges g (Edge g))
                    , Element (Edges g (Edge g)) ~ (Edge g)
                    )
                 => g -> Double -> Int -> IO (Int -> BSPM Message Int ())
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
      -- liftIO $ putStrLn $ "new estimate for " ++ (show k) ++ " " ++ (show newEst)
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
                let es = edges graph k
                ofor_ es $ \(v, w) -> do
                  let ew = w + weight
                  when (ew < spw) $ do
                    -- liftIO $ putStrLn $ "Vertext " ++ (show k) ++  " will send new estimate to " ++ (show v)
                    send v $ PathToYou k ew

runOnGraph :: ( Graph g
              , Vertex g ~ Int
              , Edge g ~ (Int, Double)
              , MonoFoldable (Edges g (Edge g))
              , Element (Edges g (Edge g)) ~ (Edge g)
              )
           => g -> Double -> Int -> Int -> IO ()
runOnGraph graph maxWeight from to = do
  liftIO $ putStrLn $ "runOnGraph: maxWeight = " ++ (show maxWeight)
                   ++ " from = " ++ (show from)
                   ++ " to = " ++ (show to)
  worker <- newVertextWorker graph maxWeight to
  run worker $ do
    send from $ PathToYou from 0
-}
