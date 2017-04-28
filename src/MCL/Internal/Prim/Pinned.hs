{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCL.Internal.Prim.Pinned where

import Data.Functor.Identity
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts
import System.IO.Unsafe

import MCL.Internal.Prim.Class
import MCL.Internal.Utils

-- | Pass pinned copy of the object to a safe C function.
{-# INLINABLE withPrimPinned #-}
withPrimPinned :: Prim t => t -> (CC t -> IO r) -> IO r
withPrimPinned fp k = do
  ByteArray ba <- pinnedByteArrayCopy $ ByteArray (prim_unwrap fp)
  k ba
  where
    pinnedByteArrayCopy :: ByteArray -> IO ByteArray
    pinnedByteArrayCopy src = do
      mdest <- newPinnedByteArray srcSize
      copyByteArray mdest 0 src 0 srcSize
      unsafeFreezeByteArray mdest
      where
        srcSize = sizeofByteArray src

-- | Use pinned memory to get a result from a safe C function.
{-# INLINABLE newPrimPinned #-}
newPrimPinned
  :: forall m t r. Prim t
  => (r -> CC t -> m t)
  -> (MC t -> IO r)
  -> IO (m t)
newPrimPinned = withPinnedByteArray1 (prim_size (proxy# :: Proxy# t))

{-# INLINABLE newPrimPinned_ #-}
newPrimPinned_ :: forall t. Prim t => (MC t -> IO ()) -> IO t
newPrimPinned_ = unwrap . newPrimPinned (\_ ba -> Identity (prim_wrap ba))
  where
    unwrap :: IO (Identity t) -> IO t
    unwrap = coerce

{-# INLINABLE maybeNewPrimPinned #-}
maybeNewPrimPinned :: Prim t => (MC t -> IO CInt) -> IO (Maybe t)
maybeNewPrimPinned = newPrimPinned $ \success ba ->
  if cintToBool success
  then Just (prim_wrap ba)
  else Nothing

----------------------------------------

{-# INLINABLE safeOp0 #-}
safeOp0 :: IO r -> r
safeOp0 = unsafeDupablePerformIO

{-# INLINABLE safeOp1 #-}
safeOp1
  :: Prim t
  => (s -> IO r)
  -> (CC t -> s)
  -> (t -> r)
safeOp1 k c_fun fa =
  safeOp0 . withPrimPinned fa $ \a -> k (c_fun a)

{-# INLINABLE safeOp1_ #-}
safeOp1_
  :: (Prim t, Prim r)
  => (CC t -> MC r -> IO ())
  -> (t -> r)
safeOp1_ = safeOp1 newPrimPinned_

{-# INLINABLE safeOp2 #-}
safeOp2
  :: (Prim t1, Prim t2)
  => (s -> IO r)
  -> (CC t1 -> CC t2 -> s)
  -> (t1 -> t2 -> r)
safeOp2 k c_fun fa fb =
  safeOp0 . withPrimPinned fa $ \a -> withPrimPinned fb $ \b -> k (c_fun a b)

{-# INLINABLE safeOp2_ #-}
safeOp2_
  :: (Prim t1, Prim t2, Prim r)
  => (CC t1 -> CC t2 -> MC r -> IO ())
  -> (t1 -> t2 -> r)
safeOp2_ = safeOp2 newPrimPinned_

----------------------------------------
-- Utils

{-# INLINABLE withPinnedByteArray1 #-}
withPinnedByteArray1
  :: Int
  -> (s -> ByteArray# -> r)
  -> (MutableByteArray# RealWorld -> IO s)
  -> IO r
withPinnedByteArray1 size k c_fun = do
  mba@(MutableByteArray umba) <- newPinnedByteArray size
  r <- c_fun umba
  ByteArray uba <- unpinnedFreezeByteArray mba
  return (k r uba)
  where
    unpinnedFreezeByteArray :: MutableByteArray RealWorld -> IO ByteArray
    unpinnedFreezeByteArray src = do
      mdest <- newByteArray srcSize
      copyMutableByteArray mdest 0 src 0 srcSize
      unsafeFreezeByteArray mdest
        where
          srcSize = sizeofMutableByteArray src
