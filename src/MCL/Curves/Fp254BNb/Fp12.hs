{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Fp12
  ( Fp12
  , beta
  , mkFp12
  , fp12_c0
  , fp12_c1
  , fp12_c2
  , fp12_c3
  , fp12_c4
  , fp12_c5
  , fp12_isZero
  -- * Internal
  , CC_Fp12
  , MC_Fp12
  , withFp12
  , newFp12
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts

import MCL.Curves.Fp254BNb.Fp2
import MCL.Internal.Utils
import qualified MCL.Internal.Field as I
import qualified MCL.Internal.Prim as I

type CC_Fp12 = ByteArray#
type MC_Fp12 = MutableByteArray# RealWorld

data Fp12 = Fp12 { unFp12 :: CC_Fp12 }

instance Binary Fp12 where
  put n = put (fp12_c0 n) *> put (fp12_c1 n) *> put (fp12_c2 n)
       *> put (fp12_c3 n) *> put (fp12_c4 n) *> put (fp12_c5 n)
  get = mkFp12 <$> get <*> get <*> get <*> get <*> get <*> get

instance NFData Fp12 where
  rnf = (`seq` ())

instance Num Fp12 where
  (+)           = I.addFp
  (-)           = I.subtractFp
  (*)           = I.multiplyFp
  negate        = I.negateFp
  abs           = I.absFp
  signum        = I.signumFp
  fromInteger n = mkFp12 (fromInteger n) 0 0 0 0 0

instance Fractional Fp12 where
  recip        = I.recipFp
  fromRational = I.fromRationalFp

instance Eq Fp12 where
  (==) = I.eqFp

instance Show Fp12 where
  showsPrec p a = showsPrec p (fp12_c0 a, fp12_c1 a, fp12_c2 a,
                               fp12_c3 a, fp12_c4 a, fp12_c5 a)

{-# NOINLINE beta #-}
beta :: Fp12
beta = mkFp12 0 0 0 1 0 0

{-# INLINABLE mkFp12 #-}
mkFp12 :: Fp2 -> Fp2 -> Fp2 -> Fp2 -> Fp2 -> Fp2 -> Fp12
mkFp12 = unsafeOp6 withFp2 newFp12 c_mcl_fp254bnb_fp12_from_base

{-# INLINABLE fp12_c0 #-}
fp12_c0 :: Fp12 -> Fp2
fp12_c0 = fp12_cX c_mcl_fp254bnb_fp12_c0

{-# INLINABLE fp12_c1 #-}
fp12_c1 :: Fp12 -> Fp2
fp12_c1 = fp12_cX c_mcl_fp254bnb_fp12_c1

{-# INLINABLE fp12_c2 #-}
fp12_c2 :: Fp12 -> Fp2
fp12_c2 = fp12_cX c_mcl_fp254bnb_fp12_c2

{-# INLINABLE fp12_c3 #-}
fp12_c3 :: Fp12 -> Fp2
fp12_c3 = fp12_cX c_mcl_fp254bnb_fp12_c3

{-# INLINABLE fp12_c4 #-}
fp12_c4 :: Fp12 -> Fp2
fp12_c4 = fp12_cX c_mcl_fp254bnb_fp12_c4

{-# INLINABLE fp12_c5 #-}
fp12_c5 :: Fp12 -> Fp2
fp12_c5 = fp12_cX c_mcl_fp254bnb_fp12_c5

{-# INLINE fp12_isZero #-}
fp12_isZero :: Fp12 -> Bool
fp12_isZero = I.isZero

----------------------------------------
-- C utils

{-# INLINE fp12_cX #-}
fp12_cX :: (CC_Fp12 -> MC_Fp2 -> IO ()) -> Fp12 -> Fp2
fp12_cX = unsafeOp1 withFp12 newFp2

{-# INLINE withFp12 #-}
withFp12 :: Fp12 -> (CC_Fp12 -> IO r) -> IO r
withFp12 = I.withPrim

{-# INLINE newFp12 #-}
newFp12 :: (MC_Fp12 -> IO ()) -> IO Fp12
newFp12 = I.newPrim_

----------------------------------------

instance I.Prim Fp12 where
  prim_size _ = fromIntegral c_mcl_fp254bnb_fp12_size
  prim_wrap   = Fp12
  prim_unwrap = unFp12

instance I.HasArith Fp12 where
  c_add      _ = c_mcl_fp254bnb_fp12_add
  c_subtract _ = c_mcl_fp254bnb_fp12_subtract
  c_multiply _ = c_mcl_fp254bnb_fp12_multiply
  c_negate   _ = c_mcl_fp254bnb_fp12_negate
  c_invert   _ = c_mcl_fp254bnb_fp12_invert
  c_eq       _ = c_mcl_fp254bnb_fp12_eq
  c_is_zero  _ = c_mcl_fp254bnb_fp12_is_zero

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_size"
  c_mcl_fp254bnb_fp12_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_add"
  c_mcl_fp254bnb_fp12_add :: CC_Fp12 -> CC_Fp12 -> MC_Fp12 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_subtract"
  c_mcl_fp254bnb_fp12_subtract :: CC_Fp12 -> CC_Fp12 -> MC_Fp12 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_multiply"
  c_mcl_fp254bnb_fp12_multiply :: CC_Fp12 -> CC_Fp12 -> MC_Fp12 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_negate"
  c_mcl_fp254bnb_fp12_negate :: CC_Fp12 -> MC_Fp12 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_from_base"
  c_mcl_fp254bnb_fp12_from_base :: CC_Fp2 -> CC_Fp2 -> CC_Fp2 -> CC_Fp2 -> CC_Fp2
                                -> CC_Fp2 -> MC_Fp12 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_invert"
  c_mcl_fp254bnb_fp12_invert :: CC_Fp12 -> MC_Fp12 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_eq"
  c_mcl_fp254bnb_fp12_eq :: CC_Fp12 -> CC_Fp12 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_c0"
  c_mcl_fp254bnb_fp12_c0 :: CC_Fp12 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_c1"
  c_mcl_fp254bnb_fp12_c1 :: CC_Fp12 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_c2"
  c_mcl_fp254bnb_fp12_c2 :: CC_Fp12 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_c3"
  c_mcl_fp254bnb_fp12_c3 :: CC_Fp12 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_c4"
  c_mcl_fp254bnb_fp12_c4 :: CC_Fp12 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_c5"
  c_mcl_fp254bnb_fp12_c5 :: CC_Fp12 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp12_is_zero"
  c_mcl_fp254bnb_fp12_is_zero :: CC_Fp12 -> IO CInt
