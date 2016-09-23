{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

module Lev.Read where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.ByteString
import           Data.ByteString.Internal
import           Data.Int
import           Data.Primitive
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Foreign.ForeignPtr
import           GHC.Exts
import           Lev.Layout

-- * Utility functions

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Lev.Get." ++ fun ++ ':':' ':msg

moduleError :: String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

data Reader m (o :: Offset) (l :: Layout) a where
   StaticReader :: (Addr -> m a) -> Reader m o ('StaticLayout s) a

bindReader :: forall m oa la a ob lb b .
      ( ob ~ (SequentialOffset oa la)
      , Monad m
      , SingI lb )
     => Reader m oa la a
     -> (a -> Reader m ob lb b)
     -> Reader m oa (SequentialLayout la lb) b
bindReader (StaticReader fa) k = case (sing :: Sing lb) of
  SStaticLayout _ -> StaticReader $ \addr -> do
    a <- fa addr
    case k a of StaticReader fb -> fb addr
  SDynamicLayout -> error "bind: lb ~ DynamicLayout"

returnReader :: forall m a o . ( Applicative m )
             => a -> Reader m o ('StaticLayout 0) a
returnReader = StaticReader . const . pure

runReader :: forall l a . ( SingI (l :: Layout ) )
          => Reader IO 0 l a -> ByteString -> IO (a, ByteString)
runReader (StaticReader f) b = case (sing :: Sing l) of
  SStaticLayout ssize -> do
    let rReq = fromInteger $ fromSing ssize
    when (bSize < rReq) $
      moduleError "runStatic" $ "Bytes remains = " ++ show bSize ++
                              ", bytes required = " ++ show rReq
    withForeignPtr bPtr $ \(Ptr bAddr) -> do
      result <- f (Addr bAddr)
      return (result, fromForeignPtr bPtr (bOff + rReq) (bSize - rReq))
  where
    (bPtr, bOff, bSize) = toForeignPtr b

prim :: forall m o a . ( PrimMonad m , Prim a, KnownNat o )
     => Reader m o ('StaticLayout (SizeOf a)) a
prim = StaticReader $ \addr -> readOffAddr (addr `plusAddr` off) 0
  where off = fromInteger $ natVal (Proxy :: Proxy o)
{-# INLINE prim #-}

int16Host :: forall m o . ( PrimMonad m, KnownNat o )
          => Reader m o ('StaticLayout (SizeOf Int16)) Int16
int16Host = prim
{-# INLINE int16Host #-}

int32Host :: forall m o . ( PrimMonad m, KnownNat o )
          => Reader m o ('StaticLayout (SizeOf Int32)) Int32
int32Host = prim
{-# INLINE int32Host #-}

int64Host :: forall m o . ( PrimMonad m, KnownNat o )
          => Reader m o ('StaticLayout (SizeOf Int64)) Int64
int64Host = prim
{-# INLINE int64Host #-}




{-
-- * Reader impl

-- ( base ptr, current addess, bytes left in buffer )
type DynamicReaderState = ( ForeignPtr Word8, Addr, Int )

data DynamicReaderResult m a = Done  !a -- done
                             | Fetch !Addr !Int !(DynamicReaderState -> m (DynamicReaderResult m a)) -- need more data (current addr, min size)

type DynamicReaderCont m a r = DynamicReaderState -> a -> m ( DynamicReaderResult m r )

-- значение смещения убрано из Layout-а. Теперь смещение должно вычисляться в ридере.
data Reader m (l :: Layout) a where
   StaticReader  :: (Addr -> m a) -> Reader m ('StaticLayout s) a
   DynamicReader :: (forall r . DynamicReaderState -> DynamicReaderCont m a r -> m (DynamicReaderResult m r)) -> Reader m 'DynamicLayout a

staticRun :: forall s a . (KnownNat s) => Reader IO ('StaticLayout s) a -> ByteString -> IO (a, ByteString)
staticRun (StaticReader f) b = do
  when (bSize < rReq) $ do
    moduleError "runStatic" $ "Bytes remains = " ++ (show bSize) ++
                              ", bytes required = " ++ (show rReq)
  withForeignPtr bPtr $ \(Ptr bAddr) -> do
    result <- f (Addr bAddr)
    return (result, fromForeignPtr bPtr (bOff + rReq) (bSize - rReq))
  where
    rReq = fromIntegral $ natVal (Proxy :: Proxy s)
    (bPtr, bOff, bSize) = toForeignPtr b
{-# INLINE staticRun #-}

-- TODO: сделать один универсальный Bind
staticBind :: forall m sa sb a b . ( Monad m )
           => (a -> Reader m ('StaticLayout sb) b)
           -> Reader m ('StaticLayout sa) a
           -> Reader m ('StaticLayout (sa + sb)) b
staticBind k (StaticReader f) = StaticReader $ \addr -> do
  a <- f addr
  case k a of StaticReader g -> g addr
{-# INLINE staticBind #-}

-- TODO: remove
(>>>=) :: forall m s1 s2 a b . ( Monad m )
       => Reader m ('StaticLayout s1) a
       -> (a -> Reader m ('StaticLayout s2) b)
       -> Reader m ('StaticLayout (s1 + s2)) b
(>>>=) = flip staticBind
{-# INLINE (>>>=) #-}

-- TODO: переименовать в просто return
staticReturn :: forall m a . ( Applicative m )
             => a -> Reader m ('StaticLayout 0) a
staticReturn = StaticReader . const . pure
{-# INLINE staticReturn #-}

-- TODO: сделать универсальный bind
dynamicBind :: (a -> Reader m 'DynamicLayout b)
            -> Reader m 'DynamicLayout a
            -> Reader m 'DynamicLayout b
dynamicBind g (DynamicReader f) = DynamicReader $ \s k ->
  f s $ \s' a -> case g a of (DynamicReader reader) -> reader s' k
{-# INLINE dynamicBind #-}


-- * Basic readers

prim :: forall m a . ( PrimMonad m, Prim a )
     => Reader m ('StaticLayout (SizeOf a)) a
prim = StaticReader $ \addr -> readOffAddr addr 0
{-# INLINE prim #-}

dynamic :: forall m a . (Monad m) => Int -> (ForeignPtr Word8 -> Addr -> m a) -> Reader m 'DynamicLayout a
dynamic len f = DynamicReader $ \s k -> do
  let go (base, addr, remains) =
        if len <= remains
          then f base addr >>= k (base, plusAddr addr len, remains - len)
          else return $ Fetch addr len go
  go s
{-# INLINE dynamic #-}

fromStatic :: forall m s a . ( Monad m, KnownNat s )
           => Reader m ('StaticLayout s) a
           -> Reader m 'DynamicLayout a
fromStatic (StaticReader f) = dynamic size $ const f
  where
    size = fromIntegral $ natVal (Proxy :: Proxy s)
{-# INLINE fromStatic #-}

-- * Derived/specialized readers

int16Host :: forall m . ( PrimMonad m )
          => Reader m ('StaticLayout (SizeOf Int16)) Int16
int16Host = prim
{-# INLINE int16Host #-}

int32Host :: forall m . ( PrimMonad m )
          => Reader m ('StaticLayout (SizeOf Int32)) Int32
int32Host = prim
{-# INLINE int32Host #-}

int64Host :: forall m . ( PrimMonad m )
          => Reader m ('StaticLayout (SizeOf Int64)) Int64
int64Host = prim
{-# INLINE int64Host #-}

byteString :: Int -> Reader IO 'DynamicLayout ByteString
byteString len = dynamic len $ \base addr -> do
  let !(Ptr baseAddr) = unsafeForeignPtrToPtr base
  return $ fromForeignPtr base (minusAddr addr (Addr baseAddr)) len
{-# INLINE byteString #-}
-}
