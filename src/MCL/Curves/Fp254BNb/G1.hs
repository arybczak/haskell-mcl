{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
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
  , newG1
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
import MCL.Curves.Fp254BNb.Fr
import MCL.Utils

type CC_G1 = ByteArray#
type MC_G1 = MutableByteArray# RealWorld

data G1 = G1 CC_G1

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
  (==) = eqG1

instance Show G1 where
  showsPrec p = maybe ("0" ++) (showsPrec p) . g1_affineCoords

instance Monoid G1 where
  mempty  = g1_zero
  mappend = plusG1

instance Group G1 where
  invert = invertG1
  pow    = flip scalarMul

instance Abelian G1

{-# INLINABLE mkG1 #-}
mkG1 :: Fp -> Fp -> Maybe G1
mkG1 = unsafeOp2 withFp maybeNewG1 c_mcl_fp254bnb_g1_construct

{-# INLINABLE mapToG1_ #-}
mapToG1_ :: Fp -> Maybe G1
mapToG1_ = unsafeOp1 withFp maybeNewG1 c_mcl_fp254bnb_g1_map_to

{-# INLINABLE mapToG1 #-}
mapToG1 :: (Fp -> Fp) -> Fp -> G1
mapToG1 f = runIdentity . mapToG1M (Identity . f)

{-# INLINABLE mapToG1M #-}
mapToG1M :: Monad m => (Fp -> m Fp) -> Fp -> m G1
mapToG1M f a = case mapToG1_ a of
  Just p  -> return p
  Nothing -> f a >>= mapToG1M f

{-# NOINLINE g1_zero #-}
g1_zero :: G1
g1_zero = unsafeOp0 $ newG1_ c_mcl_fp254bnb_g1_zero

{-# INLINABLE g1_isZero #-}
g1_isZero :: G1 -> Bool
g1_isZero = unsafeOp1 withG1 (fmap cintToBool) c_mcl_fp254bnb_g1_is_zero

{-# INLINABLE g1_affineCoords #-}
g1_affineCoords :: G1 -> Maybe (Fp, Fp)
g1_affineCoords fp
  | g1_isZero fp = Nothing
  | otherwise    = Just (unsafeOp1 withG1 new2Fp c_mcl_fp254bnb_g1_affine_coords fp)

{-# INLINABLE g1_getYfromX #-}
g1_getYfromX :: Bool -> Fp -> Maybe Fp
g1_getYfromX = unsafeOp1 withFp maybeNewFp . c_mcl_fp254bnb_g1_y_from_x . boolToCInt

{-# INLINABLE g1_powFr #-}
g1_powFr :: G1 -> Fr -> G1
g1_powFr fp fr = unsafeOp0 . withG1 fp $ \p ->
                             withFr fr $ \r ->
  newG1_ $ c_mcl_fp254bnb_g1_scalar_mul_native 1 r p

----------------------------------------
-- Internal

{-# INLINABLE eqG1 #-}
eqG1 :: G1 -> G1 -> Bool
eqG1 = unsafeOp2 withG1 (fmap cintToBool) c_mcl_fp254bnb_g1_eq

{-# INLINABLE plusG1 #-}
plusG1 :: G1 -> G1 -> G1
plusG1 = unsafeOp2 withG1 newG1_ c_mcl_fp254bnb_g1_add

{-# INLINABLE invertG1 #-}
invertG1 :: G1 -> G1
invertG1 = unsafeOp1 withG1 newG1_ c_mcl_fp254bnb_g1_invert

{-# INLINABLE scalarMul #-}
scalarMul :: Integral a => a -> G1 -> G1
scalarMul n fp = unsafeOp0 . withG1 fp $ \p -> newG1_ $ case toInteger n of
  Jp# x@(BN# ba) -> c_mcl_fp254bnb_g1_scalar_mul       1 ba (sizeofBigNat# x) 0 p
  Jn# x@(BN# ba) -> c_mcl_fp254bnb_g1_scalar_mul       1 ba (sizeofBigNat# x) 1 p
  S# k           -> c_mcl_fp254bnb_g1_scalar_mul_small 1 k p

----------------------------------------

{-# INLINABLE withG1 #-}
withG1 :: G1 -> (CC_G1 -> IO r) -> IO r
withG1 (G1 ba) k = k ba

{-# INLINABLE newG1 #-}
newG1 :: (r -> ByteArray# -> g1) -> (MC_G1 -> IO r) -> IO g1
newG1 = withByteArray1 g1Size
  where
    g1Size = fromIntegral c_mcl_fp254bnb_g1_size

{-# INLINABLE newG1_ #-}
newG1_ :: (MC_G1 -> IO ()) -> IO G1
newG1_ = newG1 (const G1)

{-# INLINABLE maybeNewG1 #-}
maybeNewG1 :: (MC_G1 -> IO CInt) -> IO (Maybe G1)
maybeNewG1 = newG1 $ \success ba -> if cintToBool success
                                    then Just (G1 ba)
                                    else Nothing

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
  c_mcl_fp254bnb_g1_scalar_mul :: CInt -> ByteArray# -> GmpSize# -> CInt
                               -> CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_scalar_mul_small"
  c_mcl_fp254bnb_g1_scalar_mul_small :: CInt -> Int# -> CC_G1 -> MC_G1 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_eq"
  c_mcl_fp254bnb_g1_eq :: CC_G1 -> CC_G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_is_zero"
  c_mcl_fp254bnb_g1_is_zero :: CC_G1 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_affine_coords"
  c_mcl_fp254bnb_g1_affine_coords :: CC_G1 -> MC_Fp -> MC_Fp ->  IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_g1_y_from_x"
  c_mcl_fp254bnb_g1_y_from_x :: CInt -> CC_Fp -> MC_Fp -> IO CInt
