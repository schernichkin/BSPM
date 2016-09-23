module Graphomania.Algo.Dijkstra
  ( Step
  , Visited
  , Graph
  , Source
  , dijkstra
  ) where

import           Control.Comonad.Cofree
import           Data.Vector.Unboxed

-- на каждом шаге раскрывается некоторое количесто вершин и некоторое количество
-- помещается в очередь открытых вершин.

data Visited = Visited
data Graph = Graph
data Source = Source

type Step m = Cofree m [Visited]


dijkstra :: Graph -> Source -> Step m
dijkstra = undefined
