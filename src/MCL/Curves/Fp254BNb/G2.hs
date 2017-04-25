{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.G2
  ( G2
  , mkG2
  , mapToG2_
  , mapToG2
  , mapToG2M
  , g2_zero
  , g2_isZero
  , g2_affineCoords
  , g2_getYfromX
  , g2_powFr
  -- * Internal
  , CC_G2
  , MC_G2
  , withG2
  , newG2
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Bits
import Data.Functor.Identity
import Data.Group
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals

import MCL.Curves.Fp254BNb.Fp
import MCL.Curves.Fp254BNb.Fp2
import MCL.Curves.Fp254BNb.Fr
import MCL.Internal.Utils

type CC_G2 = ByteArray#
type MC_G2 = MutableByteArray# RealWorld

data G2 = G2 CC_G2

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
  (==) = eqG2

instance Show G2 where
  showsPrec p = maybe ("0" ++) (showsPrec p) . g2_affineCoords

instance Monoid G2 where
  mempty  = g2_zero
  mappend = plusG2

instance Group G2 where
  invert = invertG2
  pow    = flip scalarMul

instance Abelian G2

{-# INLINABLE mkG2 #-}
mkG2 :: Fp2 -> Fp2 -> Maybe G2
mkG2 = unsafeOp2 withFp2 maybeNewG2 c_mcl_fp254bnb_g2_construct

{-# INLINABLE mapToG2_ #-}
mapToG2_ :: Fp2 -> Maybe G2
mapToG2_ = unsafeOp1 withFp2 maybeNewG2 c_mcl_fp254bnb_g2_map_to

{-# INLINABLE mapToG2 #-}
mapToG2 :: (Fp2 -> Fp2) -> Fp2 -> G2
mapToG2 f = runIdentity . mapToG2M (Identity . f)

{-# INLINABLE mapToG2M #-}
mapToG2M :: Monad m => (Fp2 -> m Fp2) -> Fp2 -> m G2
mapToG2M f a = case mapToG2_ a of
  Just p  -> return p
  Nothing -> f a >>= mapToG2M f

{-# NOINLINE g2_zero #-}
g2_zero :: G2
g2_zero = unsafeOp0 $ newG2_ c_mcl_fp254bnb_g2_zero

{-# INLINABLE g2_isZero #-}
g2_isZero :: G2 -> Bool
g2_isZero = unsafeOp1 withG2 (fmap cintToBool) c_mcl_fp254bnb_g2_is_zero

{-# INLINABLE g2_affineCoords #-}
g2_affineCoords :: G2 -> Maybe (Fp2, Fp2)
g2_affineCoords fp
  | g2_isZero fp = Nothing
  | otherwise    = Just (unsafeOp1 withG2 new2Fp2 c_mcl_fp254bnb_g2_affine_coords fp)

{-# INLINABLE g2_getYfromX #-}
g2_getYfromX :: Bool -> Fp2 -> Maybe Fp2
g2_getYfromX = unsafeOp1 withFp2 maybeNewFp2 . c_mcl_fp254bnb_g2_y_from_x . boolToCInt

{-# INLINABLE g2_powFr #-}
g2_powFr :: G2 -> Fr -> G2
g2_powFr fp fr = unsafeOp0 . withG2 fp $ \p ->
                             withFr fr $ \r ->
  newG2_ $ c_mcl_fp254bnb_g2_scalar_mul_native 1 r p

----------------------------------------
-- Internal

{-# INLINABLE eqG2 #-}
eqG2 :: G2 -> G2 -> Bool
eqG2 = unsafeOp2 withG2 (fmap cintToBool) c_mcl_fp254bnb_g2_eq

{-# INLINABLE plusG2 #-}
plusG2 :: G2 -> G2 -> G2
plusG2 = unsafeOp2 withG2 newG2_ c_mcl_fp254bnb_g2_add

{-# INLINABLE invertG2 #-}
invertG2 :: G2 -> G2
invertG2 = unsafeOp1 withG2 newG2_ c_mcl_fp254bnb_g2_invert

{-# INLINABLE scalarMul #-}
scalarMul :: Integral a => a -> G2 -> G2
scalarMul n fp = unsafeOp0 . withG2 fp $ \p -> newG2_ $ case toInteger n of
  Jp# x@(BN# ba) -> c_mcl_fp254bnb_g2_scalar_mul       1 ba (sizeofBigNat# x) 0 p
  Jn# x@(BN# ba) -> c_mcl_fp254bnb_g2_scalar_mul       1 ba (sizeofBigNat# x) 1 p
  S# k           -> c_mcl_fp254bnb_g2_scalar_mul_small 1 k p

----------------------------------------

{-# INLINABLE withG2 #-}
withG2 :: G2 -> (CC_G2 -> IO r) -> IO r
withG2 (G2 ba) k = k ba

{-# INLINABLE newG2 #-}
newG2 :: (r -> ByteArray# -> g1) -> (MC_G2 -> IO r) -> IO g1
newG2 = withByteArray1 g2Size
  where
    g2Size = fromIntegral c_mcl_fp254bnb_g2_size

{-# INLINABLE newG2_ #-}
newG2_ :: (MC_G2 -> IO ()) -> IO G2
newG2_ = newG2 (const G2)

{-# INLINABLE maybeNewG2 #-}
maybeNewG2 :: (MC_G2 -> IO CInt) -> IO (Maybe G2)
maybeNewG2 = newG2 $ \success ba -> if cintToBool success
                                    then Just (G2 ba)
                                    else Nothing

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_size"
  c_mcl_fp254bnb_g2_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_zero"
  c_mcl_fp254bnb_g2_zero :: MC_G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_construct"
  c_mcl_fp254bnb_g2_construct :: CC_Fp2 -> CC_Fp2 -> MC_G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_map_to"
  c_mcl_fp254bnb_g2_map_to :: CC_Fp2 -> MC_G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_add"
  c_mcl_fp254bnb_g2_add :: CC_G2 -> CC_G2 -> MC_G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_invert"
  c_mcl_fp254bnb_g2_invert :: CC_G2 -> MC_G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_scalar_mul_native"
  c_mcl_fp254bnb_g2_scalar_mul_native :: CInt -> CC_Fr -> CC_G2 -> MC_G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_scalar_mul"
  c_mcl_fp254bnb_g2_scalar_mul :: CInt -> ByteArray# -> GmpSize# -> CInt
                               -> CC_G2 -> MC_G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_scalar_mul_small"
  c_mcl_fp254bnb_g2_scalar_mul_small :: CInt -> Int# -> CC_G2 -> MC_G2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_eq"
  c_mcl_fp254bnb_g2_eq :: CC_G2 -> CC_G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_is_zero"
  c_mcl_fp254bnb_g2_is_zero :: CC_G2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_affine_coords"
  c_mcl_fp254bnb_g2_affine_coords :: CC_G2 -> MC_Fp2 -> MC_Fp2 ->  IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g2_y_from_x"
  c_mcl_fp254bnb_g2_y_from_x :: CInt -> CC_Fp2 -> MC_Fp2 -> IO CInt
