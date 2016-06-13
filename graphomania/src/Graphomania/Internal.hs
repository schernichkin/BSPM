module Graphomania.Internal
  (
  ) where

import           Data.Int

type VertexId = Int64
type VertexTag = Int16

type EdgeTag = Int16

type AdjacentVertex = (VertexId, EdgeTag)

class Vertex v where
  neighbors :: (Contravariant f, Applicative f)
            => (AdjacentVertex -> f AdjacentVertex) -> v -> f v
