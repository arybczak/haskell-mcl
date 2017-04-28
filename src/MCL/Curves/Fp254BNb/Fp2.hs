{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Fp2
  ( Fp2
  , alpha
  , mkFp2
  , fp2_c0
  , fp2_c1
  , fp2_isZero
  , fp2_squareRoot
  ) where

import Control.DeepSeq
import Data.Binary
import Foreign.C.Types

import MCL.Curves.Fp254BNb.Fp
import qualified MCL.Internal.Field as I
import qualified MCL.Internal.Prim as I

-- | Quadratic field extension of 'Fp' defined as @Fp(α)@, where @α² = -1@.
data Fp2 = Fp2 { unFp2 :: I.CC Fp2 }

instance Binary Fp2 where
  put n = put (fp2_c0 n) >> put (fp2_c1 n)
  get   = mkFp2 <$> get <*> get

instance NFData Fp2 where
  rnf = (`seq` ())

instance Num Fp2 where
  (+)           = I.addFp
  (-)           = I.subtractFp
  (*)           = I.multiplyFp
  negate        = I.negateFp
  abs           = I.absFp
  signum        = I.signumFp
  fromInteger n = mkFp2 (fromInteger n) 0

instance Fractional Fp2 where
  recip        = I.recipFp
  fromRational = I.fromRationalFp

instance Eq Fp2 where
  (==) = I.eqFp

instance Show Fp2 where
  showsPrec p a = case c0 of
    0 ->                 ext False
    n -> showsPrec p n . ext True
    where
      c0 = fp2_c0 a
      c1 = fp2_c1 a

      plus True  = (" + " ++)
      plus False = id

      ext out = case c1 of
        0 -> if out then id else ("0" ++)
        1 -> plus out                  . ("a" ++)
        _ -> plus out . showsPrec p c1 . ("a" ++)

-- | Root of the polynomial @x² + 1@.
{-# NOINLINE alpha #-}
alpha :: Fp2
alpha = mkFp2 0 1

-- | Construct an element of Fp from two coordinates in Fp.
{-# INLINE mkFp2 #-}
mkFp2 :: Fp -> Fp -> Fp2
mkFp2 = I.unsafeOp2_ c_mcl_fp254bnb_fp2_from_base

-- | Return first Fp coordinate of the element in Fp2.
{-# INLINE fp2_c0 #-}
fp2_c0 :: Fp2 -> Fp
fp2_c0 = I.unsafeOp1_ c_mcl_fp254bnb_fp2_c0

-- | Return second Fp coordinate of the element in Fp2.
{-# INLINE fp2_c1 #-}
fp2_c1 :: Fp2 -> Fp
fp2_c1 = I.unsafeOp1_ c_mcl_fp254bnb_fp2_c1

-- | Check whether the element of Fp2 is zero.
{-# INLINE fp2_isZero #-}
fp2_isZero :: Fp2 -> Bool
fp2_isZero = I.isZero

-- | Compute square root of the element @a ∈ Fp2@. If polynomial @x² - a@ has no
-- roots in Fp2, no result is returned.
{-# INLINE fp2_squareRoot #-}
fp2_squareRoot :: Fp2 -> Maybe Fp2
fp2_squareRoot = I.squareRoot

----------------------------------------

-- | Internal
instance I.Prim Fp2 where
  prim_size _ = fromIntegral c_mcl_fp254bnb_fp2_size
  prim_wrap   = Fp2
  prim_unwrap = unFp2

-- | Internal
instance I.HasArith Fp2 where
  c_add      _ = c_mcl_fp254bnb_fp2_add
  c_subtract _ = c_mcl_fp254bnb_fp2_subtract
  c_multiply _ = c_mcl_fp254bnb_fp2_multiply
  c_negate   _ = c_mcl_fp254bnb_fp2_negate
  c_invert   _ = c_mcl_fp254bnb_fp2_invert
  c_eq       _ = c_mcl_fp254bnb_fp2_eq
  c_is_zero  _ = c_mcl_fp254bnb_fp2_is_zero

-- | Internal
instance I.HasSqrt Fp2 where
  c_sqrt _ = c_mcl_fp254bnb_fp2_sqrt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_size"
  c_mcl_fp254bnb_fp2_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_add"
  c_mcl_fp254bnb_fp2_add :: I.CC Fp2 -> I.CC Fp2 -> I.MC Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_subtract"
  c_mcl_fp254bnb_fp2_subtract :: I.CC Fp2 -> I.CC Fp2 -> I.MC Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_multiply"
  c_mcl_fp254bnb_fp2_multiply :: I.CC Fp2 -> I.CC Fp2 -> I.MC Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_negate"
  c_mcl_fp254bnb_fp2_negate :: I.CC Fp2 -> I.MC Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_from_base"
  c_mcl_fp254bnb_fp2_from_base :: I.CC Fp -> I.CC Fp -> I.MC Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_invert"
  c_mcl_fp254bnb_fp2_invert :: I.CC Fp2 -> I.MC Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_eq"
  c_mcl_fp254bnb_fp2_eq :: I.CC Fp2 -> I.CC Fp2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_c0"
  c_mcl_fp254bnb_fp2_c0 :: I.CC Fp2 -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_c1"
  c_mcl_fp254bnb_fp2_c1 :: I.CC Fp2 -> I.MC Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_is_zero"
  c_mcl_fp254bnb_fp2_is_zero :: I.CC Fp2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_sqrt"
  c_mcl_fp254bnb_fp2_sqrt :: I.CC Fp2 -> I.MC Fp2 -> IO CInt
