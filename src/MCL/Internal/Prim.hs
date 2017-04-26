{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCL.Internal.Prim where

import Data.Functor.Identity
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts
import System.IO.Unsafe

import MCL.Internal.Utils

type CC fp = ByteArray#
type MC fp = MutableByteArray# RealWorld

class Prim t where
  prim_size   :: Proxy# t -> Int
  prim_wrap   :: CC t -> t
  prim_unwrap :: t -> CC t

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
