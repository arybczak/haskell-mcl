{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCL.Internal.Prim where

import Data.Functor.Identity
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts

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
