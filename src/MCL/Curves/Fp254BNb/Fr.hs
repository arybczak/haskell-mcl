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
  -- * Internal
  , CC_Fr
  , MC_Fr
  , withFr
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals
import qualified Data.ByteString as BS

import MCL.Internal.Utils
import qualified MCL.Internal.Field as I
import qualified MCL.Internal.Prim as I

type CC_Fr = ByteArray#
type MC_Fr = MutableByteArray# RealWorld

data Fr = Fr { unFr :: CC_Fr }

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

{-# INLINE mkFr #-}
mkFr :: Integer -> Fr
mkFr = I.mkFp

{-# INLINE hashToFr #-}
hashToFr :: BS.ByteString -> Fr
hashToFr = I.hashToFp

{-# INLINE fromFr #-}
fromFr :: Fr -> Integer
fromFr = I.fromFp

-- | Modulus of 'Fr'.
{-# NOINLINE fr_modulus #-}
fr_modulus :: Integer
fr_modulus = I.modulus (proxy# :: Proxy# Fr)

{-# INLINE fr_isZero #-}
fr_isZero :: Fr -> Bool
fr_isZero = I.isZero

{-# INLINE fr_squareRoot #-}
fr_squareRoot :: Fr -> Maybe Fr
fr_squareRoot = I.squareRoot

----------------------------------------
-- C utils

{-# INLINE withFr #-}
withFr :: Fr -> (CC_Fr -> IO r) -> IO r
withFr = I.withPrim

----------------------------------------

instance I.Prim Fr where
  prim_size _ = fromIntegral c_mcl_fp254bnb_fr_size
  prim_wrap   = Fr
  prim_unwrap = unFr

instance I.BaseField Fr where
  c_limbs        _ = fromIntegral c_mcl_fp254bnb_fr_limbs
  c_modulus      _ = c_mcl_fp254bnb_fr_modulus
  c_hash_to      _ = c_mcl_fp254bnb_fr_hash_to
  c_from_integer _ = c_mcl_fp254bnb_fr_from_integer
  c_from_hsint   _ = c_mcl_fp254bnb_fr_from_hsint
  c_to_integer   _ = c_mcl_fp254bnb_fr_to_gmp_integer

instance I.HasArith Fr where
  c_add      _ = c_mcl_fp254bnb_fr_add
  c_subtract _ = c_mcl_fp254bnb_fr_subtract
  c_multiply _ = c_mcl_fp254bnb_fr_multiply
  c_negate   _ = c_mcl_fp254bnb_fr_negate
  c_invert   _ = c_mcl_fp254bnb_fr_invert
  c_eq       _ = c_mcl_fp254bnb_fr_eq
  c_is_zero  _ = c_mcl_fp254bnb_fr_is_zero

instance I.HasSqrt Fr where
  c_sqrt _ = c_mcl_fp254bnb_fr_sqrt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_size"
  c_mcl_fp254bnb_fr_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_limbs"
  c_mcl_fp254bnb_fr_limbs :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_modulus"
  c_mcl_fp254bnb_fr_modulus :: I.MC Integer -> CSize -> IO ()

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
  c_mcl_fp254bnb_fr_from_integer :: I.CC Integer -> GmpSize# -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_from_hsint"
  c_mcl_fp254bnb_fr_from_hsint :: Int# -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_invert"
  c_mcl_fp254bnb_fr_invert :: CC_Fr -> MC_Fr -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_eq"
  c_mcl_fp254bnb_fr_eq :: CC_Fr -> CC_Fr -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_to_gmp_integer"
  c_mcl_fp254bnb_fr_to_gmp_integer :: CC_Fr -> I.MC Integer -> CSize -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_is_zero"
  c_mcl_fp254bnb_fr_is_zero :: CC_Fr -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fr_sqrt"
  c_mcl_fp254bnb_fr_sqrt :: CC_Fr -> MC_Fr -> IO CInt
