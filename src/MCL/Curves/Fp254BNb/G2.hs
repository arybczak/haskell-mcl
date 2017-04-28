{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.G2
  ( G2
  , mkG2
  , mapToG2
  , g2_zero
  , g2_isZero
  , g2_affineCoords
  , g2_getYfromX
  , g2_powFr
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Bits
import Data.Group
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals

import MCL.Curves.Fp254BNb.Fp
import MCL.Curves.Fp254BNb.Fp2
import MCL.Curves.Fp254BNb.Fr
import MCL.Internal.Utils
import qualified MCL.Internal.Group as I
import qualified MCL.Internal.Prim as I

-- | Subgroup of @E'(Fp2)@ (i.e. curve points with coordinates in Fp2) of order
-- @r@.
data G2 = G2 { unG2 :: I.CC G2 }

instance Binary G2 where
  put = putCurvePoint g2_affineCoords put
  get = getCurvePoint g2_zero mkG2

instance Binary (Compressed G2) where
  put (Compressed p) = putCurvePoint g2_affineCoords putY p
    where
      putY y = put . cintToBool . fromIntegral $ fromFp (fp2_c0 y) .&. 1

  get = getCurvePoint (Compressed g2_zero) $ \x y_lsb ->
    fmap Compressed . mkG2 x =<< g2_getYfromX y_lsb x

instance NFData G2 where
  rnf = (`seq` ())

instance Eq G2 where
  (==) = I.eqG

instance Show G2 where
  showsPrec = I.showsPrecG

-- | Point addition.
instance Monoid G2 where
  mempty  = I.zero
  mappend = I.plusG

-- | Note: 'pow' uses const-time method, just as 'g2_powFr'.
instance Group G2 where
  invert = I.invertG
  pow    = flip I.scalarMul

instance Abelian G2

-- | Construct non-zero element of G2 from two coordinates in Fp2. If @(X,Y)@
-- does not lie on G2, no result is returned.
{-# INLINABLE mkG2 #-}
mkG2
  :: Fp2 -- ^ X coordinate
  -> Fp2 -- ^ Y coordinate
  -> Maybe G2
mkG2 = I.mkG

{-# INLINABLE mapToG2 #-}
mapToG2 :: Fp2 -> G2
mapToG2 = I.mapToG

-- | Neutral element of G2 (point at infinity).
{-# NOINLINE g2_zero #-}
g2_zero :: G2
g2_zero = I.zero

-- | Check if the element of G2 is point at infinity.
{-# INLINABLE g2_isZero #-}
g2_isZero :: G2 -> Bool
g2_isZero = I.isZero

-- | Return affine coordinates of the element @a ∈ G2@. No result is returned if
-- @a@ is the point at inifinity.
{-# INLINABLE g2_affineCoords #-}
g2_affineCoords :: G2 -> Maybe (Fp2, Fp2)
g2_affineCoords = I.affineCoords

-- | Attempt to recover Y coordinate from its least significant bit and X
-- coordinate.
{-# INLINABLE g2_getYfromX #-}
g2_getYfromX
  :: Bool -- ^ Least significant bit of Y coordinate
  -> Fp2  -- ^ X coordinate
  -> Maybe Fp2
g2_getYfromX = I.getYfromX (proxy# :: Proxy# G2)

-- | Multiply the element of G2 by a scalar @x ∈ Fr@. Note: it uses const-time
-- method, i.e. the time it takes to calculate the result depends only on the
-- bitlength of @x@.
{-# INLINABLE g2_powFr #-}
g2_powFr :: G2 -> Fr -> G2
g2_powFr = I.powFr

----------------------------------------

-- | Internal
instance I.Prim G2 where
  prim_size _ = fromIntegral c_mcl_fp254bnb_g2_size
  prim_wrap   = G2
  prim_unwrap = unG2

-- | Internal
instance I.CurveGroup Fp2 G2 where
  c_zero              _ = c_mcl_fp254bnb_g2_zero
  c_construct         _ = c_mcl_fp254bnb_g2_construct
  c_map_to            _ = c_mcl_fp254bnb_g2_map_to
  c_add               _ = c_mcl_fp254bnb_g2_add
  c_invert            _ = c_mcl_fp254bnb_g2_invert
  c_scalar_mul_native _ = c_mcl_fp254bnb_g2_scalar_mul_native
  c_scalar_mul_bignat _ = c_mcl_fp254bnb_g2_scalar_mul
  c_scalar_mul_hsint  _ = c_mcl_fp254bnb_g2_scalar_mul_small
  c_eq                _ = c_mcl_fp254bnb_g2_eq
  c_is_zero           _ = c_mcl_fp254bnb_g2_is_zero
  c_affine_coords     _ = c_mcl_fp254bnb_g2_affine_coords
  c_y_from_x          _ = c_mcl_fp254bnb_g2_y_from_x

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_size"
  c_mcl_fp254bnb_g2_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_zero"
  c_mcl_fp254bnb_g2_zero :: I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_construct"
  c_mcl_fp254bnb_g2_construct :: I.CC Fp2 -> I.CC Fp2 -> I.MC G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_map_to"
  c_mcl_fp254bnb_g2_map_to :: I.CC Fp2 -> I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_add"
  c_mcl_fp254bnb_g2_add :: I.CC G2 -> I.CC G2 -> I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_invert"
  c_mcl_fp254bnb_g2_invert :: I.CC G2 -> I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_scalar_mul_native"
  c_mcl_fp254bnb_g2_scalar_mul_native :: CInt -> I.CC Fr -> I.CC G2 -> I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_scalar_mul"
  c_mcl_fp254bnb_g2_scalar_mul :: CInt -> ByteArray# -> GmpSize# -> CInt
                               -> I.CC G2 -> I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_scalar_mul_small"
  c_mcl_fp254bnb_g2_scalar_mul_small :: CInt -> Int# -> I.CC G2 -> I.MC G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_eq"
  c_mcl_fp254bnb_g2_eq :: I.CC G2 -> I.CC G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_is_zero"
  c_mcl_fp254bnb_g2_is_zero :: I.CC G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_affine_coords"
  c_mcl_fp254bnb_g2_affine_coords :: I.CC G2 -> I.MC Fp2 -> I.MC Fp2 ->  IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_y_from_x"
  c_mcl_fp254bnb_g2_y_from_x :: CInt -> I.CC Fp2 -> I.MC Fp2 -> IO CInt
