{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# LANGUAGE FlexibleContexts    #-}

module Lev.Reader
  ( Reader
  , bindReader
  , unitReader
  , runReader
  , readPrim
  , readWord8
  , readInt16Host
  , readInt32Host
  , readInt64Host
  , readByteString

  , printStatic
  , printInt32
  , printInt64
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           GHC.Exts
import           Lev.Layout

-- * Utility functions

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Reader." ++ fun ++ ':':' ':msg

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

data Reader m (l :: Layout) a where
  StaticReader :: (Addr -> m a) -> Reader m ('StaticLayout o s) a
  DynamicReader :: (forall r . DynamicReaderState -> DynamicReaderCont m a r -> m (DynamicReaderResult m r)) -> Reader m 'DynamicLayout a

type DynamicReaderState = ( ForeignPtr Word8, Addr, Int )

data DynamicReaderResult m a = Done  !a
                             | Fetch !Addr !Int !(DynamicReaderState -> m (DynamicReaderResult m a))
-- Fetch protocol:
-- Reader should only fetch when it reuqires more data than available in buffer.
-- Driver should olways return no less data than was requested by buffer.
-- Thus Fetch request during reading strict buffer indicates buffer exhaustion.

type DynamicReaderCont m a r = DynamicReaderState -> a -> m ( DynamicReaderResult m r )

bindReader :: forall m la lb a b .
      ( Monad m
      , SingI la
      , SingI lb )
     => Reader m la a
     -> (a -> Reader m lb b)
     -> Reader m (BindLayout la lb) b
bindReader (StaticReader fa) k = case (sing :: Sing lb) of
  SStaticLayout _ _ -> StaticReader $ \addr -> do
    a <- fa addr
    case k a of StaticReader fb -> fb addr
  SDynamicLayout -> DynamicReader $ \s k' -> case (sing :: Sing la) of
    SStaticLayout soff ssize -> do
      let off = fromInteger (fromSing soff)
          size = fromInteger (fromSing ssize)
          DynamicReader fa' = dynamicReader (off + size) $ const fa
      fa' s $ \s' a -> case k a of (DynamicReader fb) -> fb s' k'
bindReader (DynamicReader fa) k = case (sing :: Sing lb) of
  SStaticLayout soff ssize -> DynamicReader $ \s k' ->
    fa s $ \s' a -> case k a of
      (StaticReader fb) ->
        let off = fromInteger (fromSing soff)
            size = fromInteger (fromSing ssize)
            DynamicReader fb' = dynamicReader (off + size) $ const fb
        in fb' s' k'
  SDynamicLayout -> DynamicReader $ \s k' ->
    fa s $ \s' a -> case k a of (DynamicReader fb) -> fb s' k'
{-# INLINE bindReader #-}

unitReader :: forall m a o . ( Applicative m )
             => a -> Reader m ('StaticLayout o 0) a
unitReader = StaticReader . const . pure
{-# INLINE unitReader #-}

runReader :: forall l a . ( SingI (l :: Layout ) )
          => Reader IO l a -> ByteString -> IO (a, ByteString)
runReader (StaticReader f) b = case (sing :: Sing l) of
  SStaticLayout soff ssize -> do
    let loff = fromInteger (fromSing soff)
        rReq = loff + fromInteger (fromSing ssize)
    when (bSize < rReq) $
      moduleError "runReader" $ "Buffer size = " ++ show bSize ++
                                ", bytes required = " ++ show rReq
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
      result <- f (Addr bAddr `plusAddr` (loff + bOff))
      return (result, fromForeignPtr bPtr (bOff + rReq) (bSize - rReq))
  where
    (bPtr, bOff, bSize) = toForeignPtr b
runReader (DynamicReader f) b = withForeignPtr bPtr $ \(Ptr bAddr) -> do
  res <- f (bPtr, Addr bAddr `plusAddr` bOff, bSize) done
  case res of
    Done x -> return x
    Fetch addr req _ ->
      moduleError "runReader" $ "Buffer size = " ++ show bSize ++
                                ", bytes required = " ++ show (req + (addr `minusAddr` Addr bAddr))
  where
    done (base, addr, remains) a = do
      let !(Ptr bAddr) = unsafeForeignPtrToPtr base
          bs = fromForeignPtr bPtr (addr `minusAddr` Addr bAddr) remains
      return $ Done (a, bs)
    (bPtr, bOff, bSize) = toForeignPtr b
{-# INLINE runReader #-}

type PrimReader m o a = Reader m ('StaticLayout o (SizeOf a)) a

readPrim :: forall m o a . ( PrimMonad m , Prim a, KnownNat o ) => PrimReader m o a
readPrim = StaticReader $ \addr -> readOffAddr (addr `plusAddr` off) 0
  where off = fromInteger $ natVal (Proxy :: Proxy o)
{-# INLINE readPrim #-}

printStatic :: forall o a . ( KnownNat o, KnownNat (SizeOf a) )
            => Reader IO ('StaticLayout o (SizeOf a)) (Proxy a)
printStatic = StaticReader $ const $ do
  let off = fromInteger $ natVal (Proxy :: Proxy o)
      size = fromInteger $ natVal (Proxy :: Proxy (SizeOf a))
  Prelude.putStrLn $ show off ++ ":" ++ show size
  return (Proxy :: Proxy a)

printInt32 :: forall o . ( KnownNat o )
           => Reader IO ('StaticLayout o (SizeOf Int32)) (Proxy Int32)
printInt32 = printStatic

printInt64 :: forall o . ( KnownNat o )
           => Reader IO ('StaticLayout o (SizeOf Int64)) (Proxy Int64)
printInt64 = printStatic

readWord8 :: forall m o . ( PrimMonad m, KnownNat o ) => PrimReader m o Word8
readWord8 = readPrim
{-# INLINE readWord8 #-}

readInt16Host :: forall m o . ( PrimMonad m, KnownNat o )
              => Reader m ('StaticLayout o (SizeOf Int16)) Int16
readInt16Host = readPrim
{-# INLINE readInt16Host #-}

readInt32Host :: forall m o . ( PrimMonad m, KnownNat o )
          => Reader m ('StaticLayout o (SizeOf Int32)) Int32
readInt32Host = readPrim
{-# INLINE readInt32Host #-}

readInt64Host :: forall m o . ( PrimMonad m, KnownNat o )
          => Reader m ('StaticLayout o (SizeOf Int64)) Int64
readInt64Host = readPrim
{-# INLINE readInt64Host #-}

dynamicReader :: forall m a . (Monad m) => Int -> (ForeignPtr Word8 -> Addr -> m a) -> Reader m 'DynamicLayout a
dynamicReader len f = DynamicReader $ \s k -> do
  let go (base, addr, remains) =
        if len <= remains
          then f base addr >>= k (base, addr `plusAddr` len, remains - len)
          else return $ Fetch addr len go
  go s
{-# INLINE dynamicReader #-}

readByteString :: Int -> Reader IO 'DynamicLayout ByteString
readByteString len = dynamicReader len $ \base addr -> do
  let !(Ptr baseAddr) = unsafeForeignPtrToPtr base
  return $ fromForeignPtr base (minusAddr addr (Addr baseAddr)) len
{-# INLINE readByteString #-}
