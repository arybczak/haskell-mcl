{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.G1
  ( G1
  , mkG1
  , mapToG1_
  , mapToG1
  , mapToG1M
  , g1_zero
  , g1_isZero
  , g1_affineCoords
  , g1_getYfromX
  , g1_powFr
  -- * Internal
  , CC_G1
  , MC_G1
  , withG1
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
import MCL.Curves.Fp254BNb.Fr
import MCL.Internal.Utils
import qualified MCL.Internal.Group as I
import qualified MCL.Internal.Prim as I

type CC_G1 = ByteArray#
type MC_G1 = MutableByteArray# RealWorld

data G1 = G1 { unG1 :: CC_G1 }

instance Binary G1 where
  put = putCurvePoint g1_affineCoords put
  get = getCurvePoint g1_zero mkG1

instance Binary (Compressed G1) where
  put (Compressed p) = putCurvePoint g1_affineCoords putY p
    where
      putY y = put . cintToBool . fromIntegral $ fromFp y .&. 1

  get = getCurvePoint (Compressed g1_zero) $ \x y_lsb ->
    fmap Compressed . mkG1 x =<< g1_getYfromX y_lsb x

instance NFData G1 where
  rnf = (`seq` ())

instance Eq G1 where
  (==) = I.eqG

instance Show G1 where
  showsPrec = I.showsPrecG

instance Monoid G1 where
  mempty  = I.zero
  mappend = I.plusG

instance Group G1 where
  invert = I.invertG
  pow    = flip I.scalarMul

instance Abelian G1

{-# INLINE mkG1 #-}
mkG1 :: Fp -> Fp -> Maybe G1
mkG1 = I.mkG

{-# INLINE mapToG1_ #-}
mapToG1_ :: Fp -> Maybe G1
mapToG1_ = I.mapToG_

{-# INLINE mapToG1 #-}
mapToG1 :: (Fp -> Fp) -> Fp -> G1
mapToG1 = I.mapToG

{-# INLINE mapToG1M #-}
mapToG1M :: Monad m => (Fp -> m Fp) -> Fp -> m G1
mapToG1M = I.mapToGM

{-# NOINLINE g1_zero #-}
g1_zero :: G1
g1_zero = I.zero

{-# INLINE g1_isZero #-}
g1_isZero :: G1 -> Bool
g1_isZero = I.isZero

{-# INLINE g1_affineCoords #-}
g1_affineCoords :: G1 -> Maybe (Fp, Fp)
g1_affineCoords = I.affineCoords

{-# INLINE g1_getYfromX #-}
g1_getYfromX :: Bool -> Fp -> Maybe Fp
g1_getYfromX = I.getYfromX (proxy# :: Proxy# G1)

{-# INLINE g1_powFr #-}
g1_powFr :: G1 -> Fr -> G1
g1_powFr = I.powFr

----------------------------------------

{-# INLINE withG1 #-}
withG1 :: G1 -> (CC_G1 -> IO r) -> IO r
withG1 = I.withPrim

----------------------------------------

instance I.Prim G1 where
  prim_size _ = fromIntegral c_mcl_fp254bnb_g1_size
  prim_wrap   = G1
  prim_unwrap = unG1

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
  c_mcl_fp254bnb_g1_zero :: MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_construct"
  c_mcl_fp254bnb_g1_construct :: CC_Fp -> CC_Fp -> MC_G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_map_to"
  c_mcl_fp254bnb_g1_map_to :: CC_Fp -> MC_G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_add"
  c_mcl_fp254bnb_g1_add :: CC_G1 -> CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_invert"
  c_mcl_fp254bnb_g1_invert :: CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_scalar_mul_native"
  c_mcl_fp254bnb_g1_scalar_mul_native :: CInt -> CC_Fr -> CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_scalar_mul"
  c_mcl_fp254bnb_g1_scalar_mul :: CInt -> I.CC Integer -> GmpSize# -> CInt
                               -> CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_scalar_mul_small"
  c_mcl_fp254bnb_g1_scalar_mul_small :: CInt -> Int# -> CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_eq"
  c_mcl_fp254bnb_g1_eq :: CC_G1 -> CC_G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_is_zero"
  c_mcl_fp254bnb_g1_is_zero :: CC_G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_affine_coords"
  c_mcl_fp254bnb_g1_affine_coords :: CC_G1 -> MC_Fp -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_y_from_x"
  c_mcl_fp254bnb_g1_y_from_x :: CInt -> CC_Fp -> MC_Fp -> IO CInt
