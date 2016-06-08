{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphomania.Shumov.Binary
  (  )
  where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int
import           Data.Tagged
import           Graphomania.Shumov.Internal
import           System.Endian

instance Binary (Tagged BigEndian Int16) where
  get = Tagged <$> getInt16be
  put = putInt16be . untag

instance Binary (Tagged BigEndian Int32) where
  get = Tagged <$> getInt32be
  put = putInt32be . untag

instance Binary (Tagged BigEndian Int64) where
  get = Tagged <$> getInt64be
  put = putInt64be . untag

instance Binary (Tagged LittleEndian Int16) where
  get = Tagged <$> getInt16le
  put = putInt16le . untag

instance Binary (Tagged LittleEndian Int32) where
  get = Tagged <$> getInt32le
  put = putInt32le . untag

instance Binary (Tagged LittleEndian Int64) where
  get = Tagged <$> getInt64le
  put = putInt64le . untag

instance ( Binary (Tagged a Int16)
         , Binary (Tagged a Int64)
         ) => Binary (Tagged a ShumovEdge) where
  get = undefined

  put (Tagged ShumovEdge {..}) = do
    put (Tagged _edgeTag :: Tagged a Int16)
    put (Tagged _nodeId  :: Tagged a Int64)
