{-# LANGUAGE TypeFamilies #-}

module Data.Graph.Class where

class Graph g where
  type Vertex g
  type Edges g :: * -> *
  type Edge g

  edges :: g -> (Vertex g) -> (Edges g) (Edge g)
