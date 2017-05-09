{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Internal.Utils
  ( cintToBool
  , boolToCInt
  , putBytesFx
  , getBytesFx
  , putCurvePoint
  , getCurvePoint
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

#include <gmp.h>

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
  :: Binary x
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
