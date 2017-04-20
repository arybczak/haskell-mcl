{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Fp
  ( Fp
  , mkFp
  , hashToFp
  , fromFp
  , fp_modulus
  , fp_isZero
  , fp_squareRoot
  -- * Internal
  , CC_Fp
  , MC_Fp
  , withFp
  , newFp
  , newFp_
  , maybeNewFp
  , new2Fp
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Primitive.ByteArray
import Data.Ratio
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import MCL.Utils

type CC_Fp = ByteArray#
type MC_Fp = MutableByteArray# RealWorld

data Fp = Fp CC_Fp

instance Binary Fp where
  put = putBytesFx 32 . fromFp
  get = mkFp <$> getBytesFx 32

instance NFData Fp where
  rnf = (`seq` ())

instance Num Fp where
  (+)         = addFp
  (-)         = subtractFp
  (*)         = multiplyFp
  negate      = negateFp
  abs         = id
  signum      = \case 0 -> 0; _ -> 1
  fromInteger = mkFp

instance Fractional Fp where
  recip          = recipFp
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Eq Fp where
  (==) = eqFp

instance Show Fp where
  showsPrec p = showsPrec p . fromFp

{-# INLINABLE mkFp #-}
mkFp :: Integer -> Fp
mkFp n = unsafeOp0 . newFp_ $ case n `mod` fp_modulus of
  Jp# x@(BN# ba) -> c_mcl_fp254bnb_fp_from_integer ba (sizeofBigNat# x)
  Jn# _          -> error "fromIntegerFp: n mod p is negative"
  S# k           -> c_mcl_fp254bnb_fp_from_hsint k

{-# INLINABLE hashToFp #-}
hashToFp :: BS.ByteString -> Fp
hashToFp bs = unsafeOp0 . BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
  newFp_ (c_mcl_fp254bnb_fp_hash_to ptr $ fromIntegral len)

{-# INLINABLE fromFp #-}
fromFp :: Fp -> Integer
fromFp = unsafeOp1 withFp (importInteger fpLimbs) c_mcl_fp254bnb_fp_to_gmp_integer

{-# NOINLINE fp_modulus #-}
-- | Modulus of 'Fp'.
fp_modulus :: Integer
fp_modulus = unsafeOp0 (importInteger fpLimbs c_mcl_fp254bnb_fp_modulus)

{-# INLINABLE fp_isZero #-}
fp_isZero :: Fp -> Bool
fp_isZero = unsafeOp1 withFp (fmap cintToBool) c_mcl_fp254bnb_fp_is_zero

{-# INLINABLE fp_squareRoot #-}
fp_squareRoot :: Fp -> Maybe Fp
fp_squareRoot = unsafeOp1 withFp (newFp f) c_mcl_fp254bnb_fp_sqrt
  where
    f sqrt_exists ba = if cintToBool sqrt_exists
                       then Just (Fp ba)
                       else Nothing

----------------------------------------
-- Internal

{-# INLINABLE addFp #-}
addFp :: Fp -> Fp -> Fp
addFp = unsafeOp2 withFp newFp_ c_mcl_fp254bnb_fp_add

{-# INLINABLE subtractFp #-}
subtractFp :: Fp -> Fp -> Fp
subtractFp = unsafeOp2 withFp newFp_ c_mcl_fp254bnb_fp_subtract

{-# INLINABLE multiplyFp #-}
multiplyFp :: Fp -> Fp -> Fp
multiplyFp = unsafeOp2 withFp newFp_ c_mcl_fp254bnb_fp_multiply

{-# INLINABLE negateFp #-}
negateFp :: Fp -> Fp
negateFp = unsafeOp1 withFp newFp_ c_mcl_fp254bnb_fp_negate

{-# INLINABLE recipFp #-}
recipFp :: Fp -> Fp
recipFp = unsafeOp1 withFp newFp_ c_mcl_fp254bnb_fp_invert

{-# INLINABLE eqFp #-}
eqFp :: Fp -> Fp -> Bool
eqFp = unsafeOp2 withFp (fmap cintToBool) c_mcl_fp254bnb_fp_eq

----------------------------------------
-- C utils

{-# INLINEABLE withFp #-}
withFp :: Fp -> (CC_Fp -> IO r) -> IO r
withFp (Fp ba) k = k ba

{-# INLINEABLE newFp #-}
newFp :: (r -> ByteArray# -> fp) -> (MC_Fp -> IO r) -> IO fp
newFp = withByteArray1 fpSize

{-# INLINEABLE newFp_ #-}
newFp_ :: (MC_Fp -> IO ()) -> IO Fp
newFp_ = newFp (const Fp)

{-# INLINEABLE maybeNewFp #-}
maybeNewFp :: (MC_Fp -> IO CInt) -> IO (Maybe Fp)
maybeNewFp = newFp $ \success ba -> if cintToBool success
                                    then Just (Fp ba)
                                    else Nothing

{-# INLINEABLE new2Fp #-}
new2Fp :: (MC_Fp -> MC_Fp -> IO ()) -> IO (Fp, Fp)
new2Fp = withByteArray2 fpSize Fp

----------------------------------------

fpLimbs :: Int
fpLimbs = fromIntegral c_mcl_fp254bnb_fp_limbs

fpSize :: Int
fpSize = fromIntegral c_mcl_fp254bnb_fp_size

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_size"
  c_mcl_fp254bnb_fp_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_limbs"
  c_mcl_fp254bnb_fp_limbs :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_modulus"
  c_mcl_fp254bnb_fp_modulus :: MutableByteArray# RealWorld -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_hash_to"
  c_mcl_fp254bnb_fp_hash_to :: Ptr CChar -> CSize -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_add"
  c_mcl_fp254bnb_fp_add :: CC_Fp -> CC_Fp -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_subtract"
  c_mcl_fp254bnb_fp_subtract :: CC_Fp -> CC_Fp -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_multiply"
  c_mcl_fp254bnb_fp_multiply :: CC_Fp -> CC_Fp -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_negate"
  c_mcl_fp254bnb_fp_negate :: CC_Fp -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_from_integer"
  c_mcl_fp254bnb_fp_from_integer :: ByteArray# -> GmpSize# -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_from_hsint"
  c_mcl_fp254bnb_fp_from_hsint :: Int# -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_invert"
  c_mcl_fp254bnb_fp_invert :: CC_Fp -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_eq"
  c_mcl_fp254bnb_fp_eq :: CC_Fp -> CC_Fp -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_to_gmp_integer"
  c_mcl_fp254bnb_fp_to_gmp_integer :: CC_Fp -> MutableByteArray# RealWorld -> CSize
                                   -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_is_zero"
  c_mcl_fp254bnb_fp_is_zero :: CC_Fp -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_sqrt"
  c_mcl_fp254bnb_fp_sqrt :: CC_Fp -> MC_Fp -> IO CInt
