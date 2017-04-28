{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCL.Internal.Group where

import Data.Functor.Identity
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals

import MCL.Internal.Prim
import MCL.Internal.Utils

class (Prim fp, Prim g) => CurveGroup fp g | g -> fp where
  c_zero              :: Proxy# g -> MC g -> IO ()
  c_construct         :: Proxy# g -> CC fp -> CC fp -> MC g -> IO CInt
  c_map_to            :: Proxy# g -> CC fp -> MC g -> IO CInt
  c_add               :: Proxy# g -> CC g -> CC g -> MC g -> IO ()
  c_invert            :: Proxy# g -> CC g -> MC g -> IO ()
  c_scalar_mul_native :: Proxy# g -> CInt -> CC fr -> CC g -> MC g -> IO ()
  c_scalar_mul_bignat :: Proxy# g -> CInt -> CC Integer -> GmpSize#
                      -> CInt -> CC g -> MC g -> IO ()
  c_scalar_mul_hsint  :: Proxy# g -> CInt -> Int# -> CC g -> MC g -> IO ()
  c_eq                :: Proxy# g -> CC g -> CC g -> IO CInt
  c_is_zero           :: Proxy# g -> CC g -> IO CInt
  c_affine_coords     :: Proxy# g -> CC g -> MC fp -> MC fp -> IO ()
  c_y_from_x          :: Proxy# g -> CInt -> CC fp -> MC fp -> IO CInt

{-# INLINABLE mkG #-}
mkG :: forall fp g. CurveGroup fp g => fp -> fp -> Maybe g
mkG = unsafeOp2 maybeNewPrim $ c_construct (proxy# :: Proxy# g)

{-# INLINABLE mapToG_ #-}
mapToG_ :: forall fp g. CurveGroup fp g => fp -> Maybe g
mapToG_ = unsafeOp1 maybeNewPrim $ c_map_to (proxy# :: Proxy# g)

{-# INLINABLE mapToGM #-}
mapToGM :: (Monad m, CurveGroup fp g) => (fp -> m fp) -> fp -> m g
mapToGM f a = case mapToG_ a of
  Just p  -> return p
  Nothing -> f a >>= mapToGM f

{-# INLINABLE mapToG #-}
mapToG :: CurveGroup fp g => (fp -> fp) -> fp -> g
mapToG f = runIdentity . mapToGM (Identity . f)

{-# INLINABLE zero #-}
zero :: forall fp g. CurveGroup fp g => g
zero = unsafeOp0_ $ c_zero (proxy# :: Proxy# g)

{-# INLINABLE isZero #-}
isZero :: forall fp g. CurveGroup fp g => g -> Bool
isZero = unsafeOp1 (fmap cintToBool) $ c_is_zero (proxy# :: Proxy# g)

{-# INLINABLE affineCoords #-}
affineCoords :: forall fp g. CurveGroup fp g => g -> Maybe (fp, fp)
affineCoords fp
  | isZero fp = Nothing
  | otherwise = Just $ unsafeOp1 new2Prim (c_affine_coords (proxy# :: Proxy# g)) fp

{-# INLINABLE getYfromX #-}
getYfromX :: CurveGroup fp g => Proxy# g -> Bool -> fp -> Maybe fp
getYfromX g = unsafeOp1 maybeNewPrim . c_y_from_x g . boolToCInt

{-# INLINABLE powFr #-}
powFr :: forall fp fr g. (CurveGroup fp g, Prim fr) => g -> fr -> g
powFr = unsafeOp2_ $ \p x -> c_scalar_mul_native (proxy# :: Proxy# g) 1 x p

{-# INLINABLE eqG #-}
eqG :: forall fp g. CurveGroup fp g => g -> g -> Bool
eqG = unsafeOp2 (fmap cintToBool) $ c_eq (proxy# :: Proxy# g)

{-# INLINABLE plusG #-}
plusG :: forall fp g. CurveGroup fp g => g -> g -> g
plusG = unsafeOp2_ $ c_add (proxy# :: Proxy# g)

{-# INLINABLE invertG #-}
invertG :: forall fp g. CurveGroup fp g => g -> g
invertG = unsafeOp1_ $ c_invert (proxy# :: Proxy# g)

{-# INLINABLE scalarMul #-}
scalarMul :: forall a fp g. (CurveGroup fp g, Integral a) => a -> g -> g
scalarMul n = unsafeOp1_ $ \p -> case toInteger n of
  Jp# x@(BN# ba) -> c_scalar_mul_bignat g 1 ba (sizeofBigNat# x) 0 p
  Jn# x@(BN# ba) -> c_scalar_mul_bignat g 1 ba (sizeofBigNat# x) 1 p
  S# k           -> c_scalar_mul_hsint  g 1 k p
  where
    g = proxy# :: Proxy# g

{-# INLINABLE showsPrecG #-}
showsPrecG :: forall fp g. (CurveGroup fp g, Show fp) => Int -> g -> ShowS
showsPrecG = \p -> maybe ("0" ++) (showsPrec p) . affineCoords
