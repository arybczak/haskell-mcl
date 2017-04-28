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

-- | Prime finite field of characteristic @p@.
data Fp = Fp { unFp :: I.CC Fp }

instance Binary Fp where
  put = putBytesFx 32 . fromFp
  get = mkFp <$> getBytesFx 32

instance NFData Fp where
  rnf = (`seq` ())

instance Num Fp where
  (+)         = I.addFp
  (-)         = I.subtractFp
  (*)         = I.multiplyFp
  negate      = I.negateFp
  abs         = I.absFp
  signum      = I.signumFp
  fromInteger = I.mkFp

instance Fractional Fp where
  recip        = I.recipFp
  fromRational = I.fromRationalFp

instance Eq Fp where
  (==) = I.eqFp

instance Show Fp where
  showsPrec = I.showsPrecFp

-- | Construct an element of Fp from Integer.
{-# INLINE mkFp #-}
mkFp :: Integer -> Fp
mkFp = I.mkFp

-- | Hash arbitrary message to Fr by computing its SHA256 hash and treating its
-- first 253 bits as the value of Fr.
{-# INLINE hashToFp #-}
hashToFp :: BS.ByteString -> Fp
hashToFp = I.hashToFp

-- | Convert the element of Fp back to non-negative Integer.
{-# INLINE fromFp #-}
fromFp :: Fp -> Integer
fromFp = I.fromFp

-- | Modulus of Fp.
{-# NOINLINE fp_modulus #-}
fp_modulus :: Integer
fp_modulus = I.modulus (proxy# :: Proxy# Fp)

-- | Check if the element of Fp is zero.
{-# INLINE fp_isZero #-}
fp_isZero :: Fp -> Bool
fp_isZero = I.isZero

-- | Compute square root of the element @a ∈ Fp@. If polynomial @x² - a@ has no
-- roots in Fp, no result is returned.
{-# INLINE fp_squareRoot #-}
fp_squareRoot :: Fp -> Maybe Fp
fp_squareRoot = I.squareRoot

----------------------------------------

-- | Internal
instance I.Prim Fp where
  prim_size _ = fromIntegral c_mcl_fp254bnb_fp_size
  prim_wrap   = Fp
  prim_unwrap = unFp

-- | Internal
instance I.BaseField Fp where
  c_limbs        _ = fromIntegral c_mcl_fp254bnb_fp_limbs
  c_modulus      _ = c_mcl_fp254bnb_fp_modulus
  c_hash_to      _ = c_mcl_fp254bnb_fp_hash_to
  c_from_integer _ = c_mcl_fp254bnb_fp_from_integer
  c_from_hsint   _ = c_mcl_fp254bnb_fp_from_hsint
  c_to_integer   _ = c_mcl_fp254bnb_fp_to_gmp_integer

-- | Internal
instance I.HasArith Fp where
  c_add      _ = c_mcl_fp254bnb_fp_add
  c_subtract _ = c_mcl_fp254bnb_fp_subtract
  c_multiply _ = c_mcl_fp254bnb_fp_multiply
  c_negate   _ = c_mcl_fp254bnb_fp_negate
  c_invert   _ = c_mcl_fp254bnb_fp_invert
  c_eq       _ = c_mcl_fp254bnb_fp_eq
  c_is_zero  _ = c_mcl_fp254bnb_fp_is_zero

-- | Internal
instance I.HasSqrt Fp where
  c_sqrt _ = c_mcl_fp254bnb_fp_sqrt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_size"
  c_mcl_fp254bnb_fp_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_limbs"
  c_mcl_fp254bnb_fp_limbs :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_modulus"
  c_mcl_fp254bnb_fp_modulus :: I.MC Integer -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_hash_to"
  c_mcl_fp254bnb_fp_hash_to :: Ptr CChar -> CSize -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_add"
  c_mcl_fp254bnb_fp_add :: I.CC Fp -> I.CC Fp -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_subtract"
  c_mcl_fp254bnb_fp_subtract :: I.CC Fp -> I.CC Fp -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_multiply"
  c_mcl_fp254bnb_fp_multiply :: I.CC Fp -> I.CC Fp -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_negate"
  c_mcl_fp254bnb_fp_negate :: I.CC Fp -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_from_integer"
  c_mcl_fp254bnb_fp_from_integer :: I.CC Integer -> GmpSize# -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_from_hsint"
  c_mcl_fp254bnb_fp_from_hsint :: Int# -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_invert"
  c_mcl_fp254bnb_fp_invert :: I.CC Fp -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_eq"
  c_mcl_fp254bnb_fp_eq :: I.CC Fp -> I.CC Fp -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_to_gmp_integer"
  c_mcl_fp254bnb_fp_to_gmp_integer :: I.CC Fp -> I.MC Integer -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_is_zero"
  c_mcl_fp254bnb_fp_is_zero :: I.CC Fp -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp_sqrt"
  c_mcl_fp254bnb_fp_sqrt :: I.CC Fp -> I.MC Fp -> IO CInt
