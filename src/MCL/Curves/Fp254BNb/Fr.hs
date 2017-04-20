{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Fr
  ( Fr
  , mkFr
  , hashToFr
  , fromFr
  , fr_modulus
  , fr_isZero
  -- * Internal
  , CC_Fr
  , MC_Fr
  , withFr
  , newFr
  , newFr_
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

type CC_Fr = ByteArray#
type MC_Fr = MutableByteArray# RealWorld

data Fr = Fr CC_Fr

instance Binary Fr where
  put = putBytesFx 32 . fromFr
  get = mkFr <$> getBytesFx 32

instance NFData Fr where
  rnf = (`seq` ())

instance Num Fr where
  (+)         = addFr
  (-)         = subtractFr
  (*)         = multiplyFr
  negate      = negateFr
  abs         = id
  signum      = \case 0 -> 0; _ -> 1
  fromInteger = mkFr

instance Fractional Fr where
  recip          = recipFr
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Eq Fr where
  (==) = eqFr

instance Show Fr where
  showsPrec p = showsPrec p . fromFr

{-# INLINABLE mkFr #-}
mkFr :: Integer -> Fr
mkFr n = unsafeOp0 . newFr_ $ case n `mod` fr_modulus of
  Jp# x@(BN# ba) -> c_mcl_fp254bnb_fr_from_integer ba (sizeofBigNat# x)
  Jn# _          -> error "fromIntegerFr: n mod r is negative"
  S# k           -> c_mcl_fp254bnb_fr_from_hsint k

{-# INLINABLE hashToFr #-}
hashToFr :: BS.ByteString -> Fr
hashToFr bs = unsafeOp0 . BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
  newFr_ (c_mcl_fp254bnb_fr_hash_to ptr $ fromIntegral len)

{-# INLINABLE fromFr #-}
fromFr :: Fr -> Integer
fromFr = unsafeOp1 withFr (importInteger frLimbs) c_mcl_fp254bnb_fr_to_gmp_integer

{-# NOINLINE fr_modulus #-}
-- | Modulus of 'Fr'.
fr_modulus :: Integer
fr_modulus = unsafeOp0 (importInteger frLimbs c_mcl_fp254bnb_fr_modulus)

{-# INLINABLE fr_isZero #-}
fr_isZero :: Fr -> Bool
fr_isZero = unsafeOp1 withFr (fmap cintToBool) c_mcl_fp254bnb_fr_is_zero

----------------------------------------
-- Internal

{-# INLINABLE addFr #-}
addFr :: Fr -> Fr -> Fr
addFr = unsafeOp2 withFr newFr_ c_mcl_fp254bnb_fr_add

{-# INLINABLE subtractFr #-}
subtractFr :: Fr -> Fr -> Fr
subtractFr = unsafeOp2 withFr newFr_ c_mcl_fp254bnb_fr_subtract

{-# INLINABLE multiplyFr #-}
multiplyFr :: Fr -> Fr -> Fr
multiplyFr = unsafeOp2 withFr newFr_ c_mcl_fp254bnb_fr_multiply

{-# INLINABLE negateFr #-}
negateFr :: Fr -> Fr
negateFr = unsafeOp1 withFr newFr_ c_mcl_fp254bnb_fr_negate

{-# INLINABLE recipFr #-}
recipFr :: Fr -> Fr
recipFr = unsafeOp1 withFr newFr_ c_mcl_fp254bnb_fr_invert

{-# INLINABLE eqFr #-}
eqFr :: Fr -> Fr -> Bool
eqFr = unsafeOp2 withFr (fmap cintToBool) c_mcl_fp254bnb_fr_eq

----------------------------------------
-- C utils

{-# INLINEABLE withFr #-}
withFr :: Fr -> (CC_Fr -> IO r) -> IO r
withFr (Fr ba) k = k ba

{-# INLINEABLE newFr #-}
newFr :: (r -> ByteArray# -> fp) -> (MC_Fr -> IO r) -> IO fp
newFr = withByteArray1 frSize

{-# INLINEABLE newFr_ #-}
newFr_ :: (MC_Fr -> IO ()) -> IO Fr
newFr_ = newFr (const Fr)

----------------------------------------

frLimbs :: Int
frLimbs = fromIntegral c_mcl_fp254bnb_fr_limbs

frSize :: Int
frSize = fromIntegral c_mcl_fp254bnb_fr_size

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_size"
  c_mcl_fp254bnb_fr_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_limbs"
  c_mcl_fp254bnb_fr_limbs :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_modulus"
  c_mcl_fp254bnb_fr_modulus :: MutableByteArray# RealWorld -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_hash_to"
  c_mcl_fp254bnb_fr_hash_to :: Ptr CChar -> CSize -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_add"
  c_mcl_fp254bnb_fr_add :: CC_Fr -> CC_Fr -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_subtract"
  c_mcl_fp254bnb_fr_subtract :: CC_Fr -> CC_Fr -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_multiply"
  c_mcl_fp254bnb_fr_multiply :: CC_Fr -> CC_Fr -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_negate"
  c_mcl_fp254bnb_fr_negate :: CC_Fr -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_from_integer"
  c_mcl_fp254bnb_fr_from_integer :: ByteArray# -> GmpSize# -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_from_hsint"
  c_mcl_fp254bnb_fr_from_hsint :: Int# -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_invert"
  c_mcl_fp254bnb_fr_invert :: CC_Fr -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_eq"
  c_mcl_fp254bnb_fr_eq :: CC_Fr -> CC_Fr -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_to_gmp_integer"
  c_mcl_fp254bnb_fr_to_gmp_integer :: CC_Fr -> MutableByteArray# RealWorld -> CSize
                                   -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_is_zero"
  c_mcl_fp254bnb_fr_is_zero :: CC_Fr -> IO CInt
