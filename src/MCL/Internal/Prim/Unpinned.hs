{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCL.Internal.Prim.Unpinned where

import Data.Functor.Identity
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts
import System.IO.Unsafe

import MCL.Internal.Prim.Class
import MCL.Internal.Utils

{-# INLINABLE withPrim #-}
withPrim :: Prim t => t -> (CC t -> IO r) -> IO r
withPrim fp k = k (prim_unwrap fp)

{-# INLINABLE newPrim #-}
newPrim
  :: forall m t r. Prim t
  => (r -> CC t -> m t)
  -> (MC t -> IO r)
  -> IO (m t)
newPrim = withByteArray1 (prim_size (proxy# :: Proxy# t))

{-# INLINABLE newPrim_ #-}
newPrim_ :: forall t. Prim t => (MC t -> IO ()) -> IO t
newPrim_ = unwrap . newPrim (\_ ba -> Identity (prim_wrap ba))
  where
    unwrap :: IO (Identity t) -> IO t
    unwrap = coerce

{-# INLINABLE maybeNewPrim #-}
maybeNewPrim :: Prim t => (MC t -> IO CInt) -> IO (Maybe t)
maybeNewPrim = newPrim $ \success ba ->
  if cintToBool success
  then Just (prim_wrap ba)
  else Nothing

{-# INLINABLE new2Prim #-}
new2Prim :: forall t. Prim t => (MC t -> MC t -> IO ()) -> IO (t, t)
new2Prim = withByteArray2 (prim_size (proxy# :: Proxy# t)) prim_wrap

----------------------------------------

{-# INLINABLE unsafeOp0 #-}
unsafeOp0 :: IO r -> r
unsafeOp0 = unsafeDupablePerformIO

{-# INLINABLE unsafeOp0_ #-}
unsafeOp0_ :: Prim r => (MC r -> IO ()) -> r
unsafeOp0_ = unsafeOp0 . newPrim_

{-# INLINABLE unsafeOp1 #-}
unsafeOp1
  :: Prim t
  => (s -> IO r)
  -> (CC t -> s)
  -> (t -> r)
unsafeOp1 k c_fun fa =
  unsafeOp0 . withPrim fa $ \a -> k (c_fun a)

{-# INLINABLE unsafeOp1_ #-}
unsafeOp1_
  :: (Prim t, Prim r)
  => (CC t -> MC r -> IO ())
  -> (t -> r)
unsafeOp1_ = unsafeOp1 newPrim_

{-# INLINABLE unsafeOp2 #-}
unsafeOp2
  :: (Prim t1, Prim t2)
  => (s -> IO r)
  -> (CC t1 -> CC t2 -> s)
  -> (t1 -> t2 -> r)
unsafeOp2 k c_fun fa fb =
  unsafeOp0 . withPrim fa $ \a -> withPrim fb $ \b -> k (c_fun a b)

{-# INLINABLE unsafeOp2_ #-}
unsafeOp2_
  :: (Prim t1, Prim t2, Prim r)
  => (CC t1 -> CC t2 -> MC r -> IO ())
  -> (t1 -> t2 -> r)
unsafeOp2_ = unsafeOp2 newPrim_

{-# INLINABLE unsafeOp6 #-}
unsafeOp6
  :: (Prim t1, Prim t2, Prim t3, Prim t4, Prim t5, Prim t6)
  => (s -> IO r)
  -> (CC t1 -> CC t2 -> CC t3 -> CC t4 -> CC t5 -> CC t6 -> s)
  -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> r)
unsafeOp6 k c_fun fa fb fc fd fe ff =
  unsafeOp0 . withPrim fa $ \a -> withPrim fb $ \b -> withPrim fc $ \c ->
              withPrim fd $ \d -> withPrim fe $ \e -> withPrim ff $ \f ->
  k (c_fun a b c d e f)

{-# INLINABLE unsafeOp6_ #-}
unsafeOp6_
  :: (Prim t1, Prim t2, Prim t3, Prim t4, Prim t5, Prim t6, Prim r)
  => (CC t1 -> CC t2 -> CC t3 -> CC t4 -> CC t5 -> CC t6 -> MC r -> IO ())
  -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> r)
unsafeOp6_ = unsafeOp6 newPrim_

----------------------------------------
-- Utils

{-# INLINABLE withByteArray1 #-}
withByteArray1
  :: Int
  -> (s -> ByteArray# -> r)
  -> (MutableByteArray# RealWorld -> IO s)
  -> IO r
withByteArray1 size k c_fun = do
  mba@(MutableByteArray umba) <- newByteArray size
  r <- c_fun umba
  ByteArray uba <- unsafeFreezeByteArray mba
  return (k r uba)

{-# INLINABLE withByteArray2 #-}
withByteArray2
  :: Int
  -> (ByteArray# -> r)
  -> (MutableByteArray# RealWorld -> MutableByteArray# RealWorld -> IO ())
  -> IO (r, r)
withByteArray2 size k c_fun = do
  mba@(MutableByteArray umba) <- newByteArray size
  mbb@(MutableByteArray umbb) <- newByteArray size
  c_fun umba umbb
  ByteArray uba <- unsafeFreezeByteArray mba
  ByteArray ubb <- unsafeFreezeByteArray mbb
  return (k uba, k ubb)
