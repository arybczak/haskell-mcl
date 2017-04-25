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
  -- * Internal
  , CC_Fp2
  , MC_Fp2
  , withFp2
  , newFp2
  , maybeNewFp2
  , new2Fp2
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Exts

import MCL.Curves.Fp254BNb.Fp
import MCL.Internal.Utils
import qualified MCL.Internal.Field as I
import qualified MCL.Internal.Prim as I

type CC_Fp2 = ByteArray#
type MC_Fp2 = MutableByteArray# RealWorld

data Fp2 = Fp2 { unFp2 :: CC_Fp2 }

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

{-# NOINLINE alpha #-}
alpha :: Fp2
alpha = mkFp2 0 1

{-# INLINABLE mkFp2 #-}
mkFp2 :: Fp -> Fp -> Fp2
mkFp2 = unsafeOp2 withFp newFp2 c_mcl_fp254bnb_fp2_from_base

{-# INLINABLE fp2_c0 #-}
fp2_c0 :: Fp2 -> Fp
fp2_c0 = unsafeOp1 withFp2 newFp c_mcl_fp254bnb_fp2_c0

{-# INLINABLE fp2_c1 #-}
fp2_c1 :: Fp2 -> Fp
fp2_c1 = unsafeOp1 withFp2 newFp c_mcl_fp254bnb_fp2_c1

{-# INLINE fp2_isZero #-}
fp2_isZero :: Fp2 -> Bool
fp2_isZero = I.isZero

{-# INLINE fp2_squareRoot #-}
fp2_squareRoot :: Fp2 -> Maybe Fp2
fp2_squareRoot = I.squareRoot

----------------------------------------
-- C utils

{-# INLINE withFp2 #-}
withFp2 :: Fp2 -> (CC_Fp2 -> IO r) -> IO r
withFp2 = I.withPrim

{-# INLINE newFp2 #-}
newFp2 :: (MC_Fp2 -> IO ()) -> IO Fp2
newFp2 = I.newPrim_

{-# INLINE maybeNewFp2 #-}
maybeNewFp2 :: (MC_Fp2 -> IO CInt) -> IO (Maybe Fp2)
maybeNewFp2 = I.maybeNewPrim

{-# INLINE new2Fp2 #-}
new2Fp2 :: (MC_Fp2 -> MC_Fp2 -> IO ()) -> IO (Fp2, Fp2)
new2Fp2 = I.new2Prim

----------------------------------------

instance I.Prim Fp2 where
  prim_size _ = fromIntegral c_mcl_fp254bnb_fp2_size
  prim_wrap   = Fp2
  prim_unwrap = unFp2

instance I.HasArith Fp2 where
  c_add      _ = c_mcl_fp254bnb_fp2_add
  c_subtract _ = c_mcl_fp254bnb_fp2_subtract
  c_multiply _ = c_mcl_fp254bnb_fp2_multiply
  c_negate   _ = c_mcl_fp254bnb_fp2_negate
  c_invert   _ = c_mcl_fp254bnb_fp2_invert
  c_eq       _ = c_mcl_fp254bnb_fp2_eq
  c_is_zero  _ = c_mcl_fp254bnb_fp2_is_zero

instance I.HasSqrt Fp2 where
  c_sqrt _ = c_mcl_fp254bnb_fp2_sqrt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_size"
  c_mcl_fp254bnb_fp2_size :: CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_add"
  c_mcl_fp254bnb_fp2_add :: CC_Fp2 -> CC_Fp2 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_subtract"
  c_mcl_fp254bnb_fp2_subtract :: CC_Fp2 -> CC_Fp2 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_multiply"
  c_mcl_fp254bnb_fp2_multiply :: CC_Fp2 -> CC_Fp2 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_negate"
  c_mcl_fp254bnb_fp2_negate :: CC_Fp2 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_from_base"
  c_mcl_fp254bnb_fp2_from_base :: CC_Fp -> CC_Fp -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_invert"
  c_mcl_fp254bnb_fp2_invert :: CC_Fp2 -> MC_Fp2 -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_eq"
  c_mcl_fp254bnb_fp2_eq :: CC_Fp2 -> CC_Fp2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_c0"
  c_mcl_fp254bnb_fp2_c0 :: CC_Fp2 -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_c1"
  c_mcl_fp254bnb_fp2_c1 :: CC_Fp2 -> MC_Fp -> IO ()

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_is_zero"
  c_mcl_fp254bnb_fp2_is_zero :: CC_Fp2 -> IO CInt

foreign import ccall unsafe "hs_mcl_fp254bnb_fp2_sqrt"
  c_mcl_fp254bnb_fp2_sqrt :: CC_Fp2 -> MC_Fp2 -> IO CInt
