{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.G1
  ( G1
  , mkG1
  , mapToG1
  , g1_zero
  , g1_isZero
  , g1_affineCoords
  , g1_getYfromX
  , g1_powFr
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Bits
import Data.Group
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals

import MCL.Curves.Fp254BNb.Fp
import MCL.Curves.Fp254BNb.Fr
import MCL.Internal.Utils
import qualified MCL.Internal.Group as I
import qualified MCL.Internal.Prim as I

-- | Subgroup of @E(Fp)@ (i.e. curve points with coordinates in Fp) of order
-- @r@.
data G1 = G1 { unG1 :: I.CC G1 }

instance Binary G1 where
  put = putCurvePoint g1_affineCoords putY
    where
      putY y = put . cintToBool . fromIntegral $ fromFp y .&. 1

  get = getCurvePoint g1_zero $ \x y_lsb ->
    mkG1 x =<< g1_getYfromX y_lsb x

instance NFData G1 where
  rnf = (`seq` ())

instance Eq G1 where
  (==) = I.eqG

instance Show G1 where
  showsPrec = I.showsPrecG

-- | Point addition.
instance Monoid G1 where
  mempty  = I.zero
  mappend = I.plusG

-- | Note: 'pow' uses const-time method, just as 'g1_powFr'.
instance Group G1 where
  invert = I.invertG
  pow    = flip I.scalarMul

instance Abelian G1

-- | Construct non-zero element of G1 from two coordinates in Fp. If @(X,Y)@
-- does not lie on G1, no result is returned.
{-# INLINE mkG1 #-}
mkG1
  :: Fp -- ^ X coordinate
  -> Fp -- ^ Y coordinate
  -> Maybe G1
mkG1 = I.mkG

{-# INLINE mapToG1 #-}
mapToG1 :: Fp -> G1
mapToG1 = I.mapToG

-- | Neutral element of G1 (point at infinity).
{-# NOINLINE g1_zero #-}
g1_zero :: G1
g1_zero = I.zero

-- | Check if the element of G1 is point at infinity.
{-# INLINE g1_isZero #-}
g1_isZero :: G1 -> Bool
g1_isZero = I.isZero

-- | Return affine coordinates of the element @a ∈ G1@. No result is returned if
-- @a@ is the point at inifinity.
{-# INLINE g1_affineCoords #-}
g1_affineCoords :: G1 -> Maybe (Fp, Fp)
g1_affineCoords = I.affineCoords

-- | Attempt to recover Y coordinate from its least significant bit and X
-- coordinate.
{-# INLINE g1_getYfromX #-}
g1_getYfromX
  :: Bool -- ^ Least significant bit of Y coordinate
  -> Fp   -- ^ X coordinate
  -> Maybe Fp
g1_getYfromX = I.getYfromX (proxy# :: Proxy# G1)

-- | Multiply the element of G1 by a scalar @x ∈ Fr@. Note: it uses const-time
-- method, i.e. the time it takes to calculate the result depends only on the
-- bitlength of @x@.
{-# INLINE g1_powFr #-}
g1_powFr :: G1 -> Fr -> G1
g1_powFr = I.powFr

----------------------------------------

-- | Internal
instance I.Prim G1 where
  prim_size _ = fromIntegral c_mcl_fp254bnb_g1_size
  prim_wrap   = G1
  prim_unwrap = unG1

-- | Internal
instance I.CurveGroup Fp G1 where
  c_zero              _ = c_mcl_fp254bnb_g1_zero
  c_construct         _ = c_mcl_fp254bnb_g1_construct
  c_map_to            _ = c_mcl_fp254bnb_g1_map_to
  c_add               _ = c_mcl_fp254bnb_g1_add
  c_invert            _ = c_mcl_fp254bnb_g1_invert
  c_scalar_mul_native _ = c_mcl_fp254bnb_g1_scalar_mul_native
  c_scalar_mul_bignat _ = c_mcl_fp254bnb_g1_scalar_mul
  c_scalar_mul_hsint  _ = c_mcl_fp254bnb_g1_scalar_mul_small
  c_eq                _ = c_mcl_fp254bnb_g1_eq
  c_is_zero           _ = c_mcl_fp254bnb_g1_is_zero
  c_affine_coords     _ = c_mcl_fp254bnb_g1_affine_coords
  c_y_from_x          _ = c_mcl_fp254bnb_g1_y_from_x

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_size"
  c_mcl_fp254bnb_g1_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_zero"
  c_mcl_fp254bnb_g1_zero :: I.MC G1 -> IO ()

foreign import ccall safe "hs_mcl_fp254bnb_g1_construct"
  c_mcl_fp254bnb_g1_construct :: I.CC Fp -> I.CC Fp -> I.MC G1 -> IO CInt

foreign import ccall safe "hs_mcl_fp254bnb_g1_map_to"
  c_mcl_fp254bnb_g1_map_to :: I.CC Fp -> I.MC G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_add"
  c_mcl_fp254bnb_g1_add :: I.CC G1 -> I.CC G1 -> I.MC G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_invert"
  c_mcl_fp254bnb_g1_invert :: I.CC G1 -> I.MC G1 -> IO ()

foreign import ccall safe "hs_mcl_fp254bnb_g1_scalar_mul_native"
  c_mcl_fp254bnb_g1_scalar_mul_native :: CInt -> I.CC Fr -> I.CC G1 -> I.MC G1 -> IO ()

foreign import ccall safe "hs_mcl_fp254bnb_g1_scalar_mul"
  c_mcl_fp254bnb_g1_scalar_mul :: CInt -> I.CC Integer -> GmpSize# -> CInt
                               -> I.CC G1 -> I.MC G1 -> IO ()

foreign import ccall safe "hs_mcl_fp254bnb_g1_scalar_mul_small"
  c_mcl_fp254bnb_g1_scalar_mul_small :: CInt -> Int# -> I.CC G1 -> I.MC G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_eq"
  c_mcl_fp254bnb_g1_eq :: I.CC G1 -> I.CC G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_is_zero"
  c_mcl_fp254bnb_g1_is_zero :: I.CC G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_affine_coords"
  c_mcl_fp254bnb_g1_affine_coords :: I.CC G1 -> I.MC Fp -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_y_from_x"
  c_mcl_fp254bnb_g1_y_from_x :: CInt -> I.CC Fp -> I.MC Fp -> IO CInt
