{-# LANGUAGE MagicHash #-}
module MCL.Internal.Prim.Class where

import Data.Primitive.ByteArray
import GHC.Exts

-- | C representation of a const MCL object or Integer.
type CC t = ByteArray#

-- | C representation of a mutable MCL object or Integer.
type MC t = MutableByteArray# RealWorld

class Prim t where
  prim_size   :: Proxy# t -> Int
  prim_wrap   :: CC t -> t
  prim_unwrap :: t -> CC t
