module Graphomania.Shumov.Internal
    ( Shumov (..)
    , ShumovEdge (..)
    , ShumovVertex (..)
    ) where

import           Data.ByteString
import           Data.Int


-- TODO: представляения для BE и LE совпадают, но различаются кодеры/декодеры
-- возможно сделует добавить

newtype Shumov = Shumov { unShumov :: ByteString }

data ShumovVertex = ShumovVertex
  { _externalId :: !ByteString
  } deriving ( Show, Eq, Ord ) -- TODO: redefine Show, Eq, Ord classes.

data ShumovEdge = ShumovEdge
  { _edgeTag :: !Int16
  , _nodeId  :: !Int64
  } deriving ( Show, Eq, Ord )
