{-# LANGUAGE LambdaCase #-}
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
  , newFp2_
  , maybeNewFp2
  , new2Fp2
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Primitive.ByteArray
import Data.Ratio
import Foreign.C.Types
import GHC.Exts

import MCL.Curves.Fp254BNb.Fp
import MCL.Utils

type CC_Fp2 = ByteArray#
type MC_Fp2 = MutableByteArray# RealWorld

data Fp2 = Fp2 CC_Fp2

instance Binary Fp2 where
  put n = put (fp2_c0 n) >> put (fp2_c1 n)
  get   = mkFp2 <$> get <*> get

instance NFData Fp2 where
  rnf = (`seq` ())

instance Num Fp2 where
  (+)           = addFp2
  (-)           = subtractFp2
  (*)           = multiplyFp2
  negate        = negateFp2
  abs           = id
  signum        = \case 0 -> 0; _ -> 1
  fromInteger n = mkFp2 (fromInteger n) 0

instance Fractional Fp2 where
  recip          = recipFp2
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Eq Fp2 where
  (==) = eqFp2

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
mkFp2 = unsafeOp2 withFp newFp2_ c_mcl_fp254bnb_fp2_from_base

{-# INLINABLE fp2_c0 #-}
fp2_c0 :: Fp2 -> Fp
fp2_c0 = unsafeOp1 withFp2 newFp_ c_mcl_fp254bnb_fp2_c0

{-# INLINABLE fp2_c1 #-}
fp2_c1 :: Fp2 -> Fp
fp2_c1 = unsafeOp1 withFp2 newFp_ c_mcl_fp254bnb_fp2_c1

{-# INLINABLE fp2_isZero #-}
fp2_isZero :: Fp2 -> Bool
fp2_isZero = unsafeOp1 withFp2 (fmap cintToBool) c_mcl_fp254bnb_fp2_is_zero

{-# INLINABLE fp2_squareRoot #-}
fp2_squareRoot :: Fp2 -> Maybe Fp2
fp2_squareRoot = unsafeOp1 withFp2 maybeNewFp2 c_mcl_fp254bnb_fp2_sqrt

----------------------------------------
-- Internal

{-# INLINABLE addFp2 #-}
addFp2 :: Fp2 -> Fp2 -> Fp2
addFp2 = unsafeOp2 withFp2 newFp2_ c_mcl_fp254bnb_fp2_add

{-# INLINABLE subtractFp2 #-}
subtractFp2 :: Fp2 -> Fp2 -> Fp2
subtractFp2 = unsafeOp2 withFp2 newFp2_ c_mcl_fp254bnb_fp2_subtract

{-# INLINABLE multiplyFp2 #-}
multiplyFp2 :: Fp2 -> Fp2 -> Fp2
multiplyFp2 = unsafeOp2 withFp2 newFp2_ c_mcl_fp254bnb_fp2_multiply

{-# INLINABLE negateFp2 #-}
negateFp2 :: Fp2 -> Fp2
negateFp2 = unsafeOp1 withFp2 newFp2_ c_mcl_fp254bnb_fp2_negate

{-# INLINABLE recipFp2 #-}
recipFp2 :: Fp2 -> Fp2
recipFp2 = unsafeOp1 withFp2 newFp2_ c_mcl_fp254bnb_fp2_invert

{-# INLINABLE eqFp2 #-}
eqFp2 :: Fp2 -> Fp2 -> Bool
eqFp2 = unsafeOp2 withFp2 (fmap cintToBool) c_mcl_fp254bnb_fp2_eq

----------------------------------------
-- C utils

{-# INLINEABLE withFp2 #-}
withFp2 :: Fp2 -> (CC_Fp2 -> IO r) -> IO r
withFp2 (Fp2 ba) k = k ba

{-# INLINEABLE newFp2 #-}
newFp2 :: (r -> ByteArray# -> fp2) -> (MC_Fp2 -> IO r) -> IO fp2
newFp2 = withByteArray1 fp2Size

{-# INLINABLE newFp2_ #-}
newFp2_ :: (MC_Fp2 -> IO r) -> IO Fp2
newFp2_ = newFp2 (const Fp2)

{-# INLINABLE maybeNewFp2 #-}
maybeNewFp2 :: (MC_Fp2 -> IO CInt) -> IO (Maybe Fp2)
maybeNewFp2 = newFp2 $ \success ba -> if cintToBool success
                                      then Just (Fp2 ba)
                                      else Nothing

{-# INLINEABLE new2Fp2 #-}
new2Fp2 :: (MC_Fp2 -> MC_Fp2 -> IO ()) -> IO (Fp2, Fp2)
new2Fp2 = withByteArray2 fp2Size Fp2

----------------------------------------

fp2Size :: Int
fp2Size = fromIntegral c_mcl_fp254bnb_fp2_size

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
