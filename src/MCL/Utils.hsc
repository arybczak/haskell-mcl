{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Utils
  ( Compressed(..)
  , cintToBool
  , boolToCInt
  , putBytesFx
  , getBytesFx
  , putCurvePoint
  , getCurvePoint
  , withByteArray1
  , withByteArray2
  , unsafeOp0
  , unsafeOp1
  , unsafeOp2
  , unsafeOp6
  , importInteger
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List
import Data.Primitive.ByteArray
import Data.Typeable
import Foreign.C.Types
import GHC.Exts
import GHC.Integer.GMP.Internals
import System.IO.Unsafe

#include <gmp.h>

newtype Compressed g = Compressed { unCompressed :: g }
  deriving (Eq, NFData, Show, Typeable)

{-# INLINE cintToBool #-}
cintToBool :: CInt -> Bool
cintToBool 0 = False
cintToBool _ = True

{-# INLINE boolToCInt #-}
boolToCInt :: Bool -> CInt
boolToCInt False = 0
boolToCInt True  = 1

{-# INLINABLE putBytesFx #-}
putBytesFx :: Int -> Integer -> Put
putBytesFx 0 _ = return ()
putBytesFx n k = do
  putWord64le (fromIntegral k)
  putBytesFx (n - 8) (k `shiftR` 64)

{-# INLINABLE getBytesFx #-}
getBytesFx :: Int -> Get Integer
getBytesFx n = foldl' assemble 0 <$> collect [] (n `div` 8)
  where
    collect acc 0 = return acc
    collect acc k = do
      w <- getWord64le
      collect (w : acc) (k - 1)

    assemble :: Integer -> Word64 -> Integer
    assemble acc w = acc `shiftL` 64 .|. fromIntegral w

{-# INLINABLE putCurvePoint #-}
putCurvePoint
  :: (Binary x, Binary y)
  => (g -> Maybe (x, y))
  -> (y -> Put)
  -> g
  -> Put
putCurvePoint affineCoords putY p = case affineCoords p of
  Nothing     -> putWord8 0
  Just (x, y) -> putWord8 1 >> put x >> putY y

{-# INLINABLE getCurvePoint #-}
getCurvePoint
  :: (Typeable g, Binary x, Binary y)
  => g
  -> (x -> y -> Maybe g)
  -> Get g
getCurvePoint zero mkG = getWord8 >>= \case
  0 -> return zero
  1 -> do
    x <- get
    y <- get
    case mkG x y of
      Nothing -> fail $ errPrefix ++ "invalid point"
      Just p  -> return p
  n -> fail $ errPrefix ++ "expected 0 or 1, got " ++ show n
  where
    errPrefix = "getCurvePoint (" ++ show (typeOf zero) ++ "): "

----------------------------------------

{-# INLINABLE withByteArray1 #-}
withByteArray1
  :: Int
  -> (s -> ByteArray## -> r)
  -> (MutableByteArray## RealWorld -> IO s)
  -> IO r
withByteArray1 size k c_fun = do
  mba@(MutableByteArray umba) <- newByteArray size
  r <- c_fun umba
  ByteArray uba <- unsafeFreezeByteArray mba
  return (k r uba)

{-# INLINABLE withByteArray2 #-}
withByteArray2
  :: Int
  -> (ByteArray## -> r)
  -> (MutableByteArray## RealWorld -> MutableByteArray## RealWorld -> IO ())
  -> IO (r, r)
withByteArray2 size k c_fun = do
  mba@(MutableByteArray umba) <- newByteArray size
  mbb@(MutableByteArray umbb) <- newByteArray size
  c_fun umba umbb
  ByteArray uba <- unsafeFreezeByteArray mba
  ByteArray ubb <- unsafeFreezeByteArray mbb
  return (k uba, k ubb)

----------------------------------------

{-# INLINABLE unsafeOp0 #-}
unsafeOp0 :: IO r -> r
unsafeOp0 = unsafeDupablePerformIO

{-# INLINABLE unsafeOp1 #-}
unsafeOp1
  :: (t -> (ByteArray## -> IO r) -> IO r)
  -> (s -> IO r)
  -> (ByteArray## -> s)
  -> (t -> r)
unsafeOp1 with k c_fun fa =
  unsafeOp0 . with fa $ \a -> k (c_fun a)

{-# INLINABLE unsafeOp2 #-}
unsafeOp2
  :: (t -> (ByteArray## -> IO r) -> IO r)
  -> (s -> IO r)
  -> (ByteArray## -> ByteArray## -> s)
  -> (t -> t -> r)
unsafeOp2 with k c_fun fa fb =
  unsafeOp0 . with fa $ \a -> with fb $ \b -> k (c_fun a b)

{-# INLINABLE unsafeOp6 #-}
unsafeOp6
  :: (t -> (ByteArray## -> IO r) -> IO r)
  -> (s -> IO r)
  -> (ByteArray## -> ByteArray## -> ByteArray## ->
      ByteArray## -> ByteArray## -> ByteArray## -> s)
  -> (t -> t -> t -> t -> t -> t -> r)
unsafeOp6 with k c_fun fa fb fc fd fe ff =
  unsafeOp0 . with fa $ \a -> with fb $ \b -> with fc $ \c ->
              with fd $ \d -> with fe $ \e -> with ff $ \f ->
  k (c_fun a b c d e f)

{-# INLINABLE importInteger #-}
importInteger
  :: Int
  -> (MutableByteArray## RealWorld -> CSize -> IO ())
  -> IO Integer
importInteger limbs c_fun = do
  mba@(MutableByteArray umba) <- newByteArray size
  let csize = fromIntegral size
  c_fun umba csize
  ByteArray uba <- unsafeFreezeByteArray mba
  return $ Jp## (BN## uba)
  where
    size = limbs * #{size mp_limb_t}
