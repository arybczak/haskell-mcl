{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Fr
  ( Fr
  , mkFr
  , hashToFr
  , fromFr
  , fr_modulus
  , fr_isZero
  , fr_squareRoot
  ) where

import Control.DeepSeq
import Data.Binary
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals
import qualified Data.ByteString as BS

import MCL.Internal.Utils
import qualified MCL.Internal.Field as I
import qualified MCL.Internal.Prim as I

-- | Prime finite field of characteristic @r@.
data Fr = Fr { unFr :: I.CC Fr }

instance Binary Fr where
  put = putBytesFx 32 . fromFr
  get = mkFr <$> getBytesFx 32

instance NFData Fr where
  rnf = (`seq` ())

instance Num Fr where
  (+)         = I.addFp
  (-)         = I.subtractFp
  (*)         = I.multiplyFp
  negate      = I.negateFp
  abs         = I.absFp
  signum      = I.signumFp
  fromInteger = mkFr

instance Fractional Fr where
  recip        = I.recipFp
  fromRational = I.fromRationalFp

instance Eq Fr where
  (==) = I.eqFp

instance Show Fr where
  showsPrec = I.showsPrecFp

-- | Construct an element of Fr from Integer.
{-# INLINE mkFr #-}
mkFr :: Integer -> Fr
mkFr = I.mkFp

-- | Hash arbitrary message to Fr by computing its SHA256 hash and treating its
-- first 253 bits as the value of Fr.
{-# INLINE hashToFr #-}
hashToFr :: BS.ByteString -> Fr
hashToFr = I.hashToFp

-- | Convert the element of Fr back to non-negative Integer.
{-# INLINE fromFr #-}
fromFr :: Fr -> Integer
fromFr = I.fromFp

-- | Modulus of Fr.
{-# NOINLINE fr_modulus #-}
fr_modulus :: Integer
fr_modulus = I.modulus (proxy# :: Proxy# Fr)

-- | Check if the element of Fr is zero.
{-# INLINE fr_isZero #-}
fr_isZero :: Fr -> Bool
fr_isZero = I.isZero

-- | Compute square root of the element @a ∈ Fr@. If polynomial @x² - a@ has no
-- roots in Fr, no result is returned.
{-# INLINE fr_squareRoot #-}
fr_squareRoot :: Fr -> Maybe Fr
fr_squareRoot = I.squareRoot

----------------------------------------

-- | Internal
instance I.Prim Fr where
  prim_size _ = fromIntegral c_mcl_fp254bnb_fr_size
  prim_wrap   = Fr
  prim_unwrap = unFr

-- | Internal
instance I.BaseField Fr where
  c_limbs        _ = fromIntegral c_mcl_fp254bnb_fr_limbs
  c_modulus      _ = c_mcl_fp254bnb_fr_modulus
  c_hash_to      _ = c_mcl_fp254bnb_fr_hash_to
  c_from_integer _ = c_mcl_fp254bnb_fr_from_integer
  c_from_hsint   _ = c_mcl_fp254bnb_fr_from_hsint
  c_to_integer   _ = c_mcl_fp254bnb_fr_to_gmp_integer

-- | Internal
instance I.HasArith Fr where
  c_add      _ = c_mcl_fp254bnb_fr_add
  c_subtract _ = c_mcl_fp254bnb_fr_subtract
  c_multiply _ = c_mcl_fp254bnb_fr_multiply
  c_negate   _ = c_mcl_fp254bnb_fr_negate
  c_invert   _ = c_mcl_fp254bnb_fr_invert
  c_eq       _ = c_mcl_fp254bnb_fr_eq
  c_is_zero  _ = c_mcl_fp254bnb_fr_is_zero

-- | Internal
instance I.HasSqrt Fr where
  c_sqrt _ = c_mcl_fp254bnb_fr_sqrt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_size"
  c_mcl_fp254bnb_fr_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_limbs"
  c_mcl_fp254bnb_fr_limbs :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_modulus"
  c_mcl_fp254bnb_fr_modulus :: I.MC Integer -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_hash_to"
  c_mcl_fp254bnb_fr_hash_to :: Ptr CChar -> CSize -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_add"
  c_mcl_fp254bnb_fr_add :: I.CC Fr -> I.CC Fr -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_subtract"
  c_mcl_fp254bnb_fr_subtract :: I.CC Fr -> I.CC Fr -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_multiply"
  c_mcl_fp254bnb_fr_multiply :: I.CC Fr -> I.CC Fr -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_negate"
  c_mcl_fp254bnb_fr_negate :: I.CC Fr -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_from_integer"
  c_mcl_fp254bnb_fr_from_integer :: I.CC Integer -> GmpSize# -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_from_hsint"
  c_mcl_fp254bnb_fr_from_hsint :: Int# -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_invert"
  c_mcl_fp254bnb_fr_invert :: I.CC Fr -> I.MC Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_eq"
  c_mcl_fp254bnb_fr_eq :: I.CC Fr -> I.CC Fr -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_to_gmp_integer"
  c_mcl_fp254bnb_fr_to_gmp_integer :: I.CC Fr -> I.MC Integer -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_is_zero"
  c_mcl_fp254bnb_fr_is_zero :: I.CC Fr -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_sqrt"
  c_mcl_fp254bnb_fr_sqrt :: I.CC Fr -> I.MC Fr -> IO CInt
