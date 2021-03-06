{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCL.Internal.Field where

import Data.Ratio
import Data.Typeable
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import MCL.Internal.Prim
import MCL.Internal.Utils

class Prim fp => BaseField fp where
  c_limbs        :: Proxy# fp -> Int
  c_modulus      :: Proxy# fp -> MC Integer -> CSize -> IO ()
  c_hash_to      :: Proxy# fp -> Ptr CChar -> CSize -> MC fp -> IO ()
  c_from_integer :: Proxy# fp -> CC Integer -> GmpSize# -> MC fp -> IO ()
  c_from_hsint   :: Proxy# fp -> Int# -> MC fp -> IO ()
  c_to_integer   :: Proxy# fp -> CC fp -> MC Integer -> CSize -> IO ()

{-# INLINABLE mkFp #-}
mkFp :: forall fp. (BaseField fp, Typeable fp) => Integer -> fp
mkFp n = unsafeOp0_ $ case n `mod` (modulus fp) of
  Jp# x@(BN# ba) -> c_from_integer fp ba (sizeofBigNat# x)
  Jn# _          -> error $ "mkFp (" ++ fpRep ++ "): n mod p is negative"
  S# k           -> c_from_hsint fp k
  where
    fp    = proxy# :: Proxy# fp
    fpRep = show $ typeRep (Proxy :: Proxy fp)

{-# INLINABLE hashToFp #-}
hashToFp :: forall fp. BaseField fp => BS.ByteString -> fp
hashToFp bs = unsafeOp0 . BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
  newPrim_ $ c_hash_to (proxy# :: Proxy# fp) ptr (fromIntegral len)

{-# INLINABLE fromFp #-}
fromFp :: forall fp. BaseField fp => fp -> Integer
fromFp = unsafeOp1 (importInteger (c_limbs fp)) (c_to_integer fp)
  where
    fp = proxy# :: Proxy# fp

{-# INLINABLE modulus #-}
modulus :: BaseField fp => Proxy# fp -> Integer
modulus fp = unsafeOp0 $ importInteger (c_limbs fp) (c_modulus fp)

{-# INLINABLE showsPrecFp #-}
showsPrecFp :: BaseField fp => Int -> fp -> ShowS
showsPrecFp p = showsPrec p . fromFp

----------------------------------------

class Prim fp => HasArith fp where
  c_add      :: Proxy# fp -> CC fp -> CC fp -> MC fp -> IO ()
  c_subtract :: Proxy# fp -> CC fp -> CC fp -> MC fp -> IO ()
  c_multiply :: Proxy# fp -> CC fp -> CC fp -> MC fp -> IO ()
  c_negate   :: Proxy# fp -> CC fp          -> MC fp -> IO ()
  c_invert   :: Proxy# fp -> CC fp          -> MC fp -> IO ()
  c_eq       :: Proxy# fp -> CC fp -> CC fp          -> IO CInt
  c_is_zero  :: Proxy# fp -> CC fp                   -> IO CInt

{-# INLINABLE isZero #-}
isZero :: forall fp. HasArith fp => fp -> Bool
isZero = unsafeOp1 (fmap cintToBool) $ c_is_zero (proxy# :: Proxy# fp)

{-# INLINABLE addFp #-}
addFp :: forall fp. HasArith fp => fp -> fp -> fp
addFp = unsafeOp2_ $ c_add (proxy# :: Proxy# fp)

{-# INLINABLE subtractFp #-}
subtractFp :: forall fp. HasArith fp => fp -> fp -> fp
subtractFp = unsafeOp2_ $ c_subtract (proxy# :: Proxy# fp)

{-# INLINABLE multiplyFp #-}
multiplyFp :: forall fp. HasArith fp => fp -> fp -> fp
multiplyFp = unsafeOp2_ $ c_multiply (proxy# :: Proxy# fp)

{-# INLINABLE negateFp #-}
negateFp :: forall fp. HasArith fp => fp -> fp
negateFp = unsafeOp1_ $ c_negate (proxy# :: Proxy# fp)

{-# INLINABLE absFp #-}
absFp :: fp -> fp
absFp = id

{-# INLINABLE signumFp #-}
signumFp :: forall fp. (HasArith fp, Num fp) => fp -> fp
signumFp fp = if isZero fp then fp else 1

{-# INLINABLE recipFp #-}
recipFp :: forall fp. HasArith fp => fp -> fp
recipFp = unsafeOp1_ $ c_invert (proxy# :: Proxy# fp)

{-# INLINABLE fromRationalFp #-}
fromRationalFp :: Fractional fp => Rational -> fp
fromRationalFp r = fromIntegral (numerator r) / fromIntegral (denominator r)

{-# INLINABLE eqFp #-}
eqFp :: forall fp. HasArith fp => fp -> fp -> Bool
eqFp = unsafeOp2 (fmap cintToBool) $ c_eq (proxy# :: Proxy# fp)

----------------------------------------

class HasArith fp => HasSqrt fp where
  c_sqrt :: Proxy# fp -> CC fp -> MC fp -> IO CInt

{-# INLINABLE squareRoot #-}
squareRoot :: forall fp. HasSqrt fp => fp -> Maybe fp
squareRoot = unsafeOp1 maybeNewPrim $ c_sqrt (proxy# :: Proxy# fp)
