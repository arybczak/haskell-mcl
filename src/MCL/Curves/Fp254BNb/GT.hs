{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MCL.Curves.Fp254BNb.GT
  ( GT(..)
  , mkGT
  , gt_powFr
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Group

import MCL.Curves.Fp254BNb.Fp12
import MCL.Curves.Fp254BNb.Fr

newtype GT = GT_ Fp12
  deriving (Binary, Eq, NFData)

instance Show GT where
  showsPrec p (GT_ a) = showsPrec p a

instance Monoid GT where
  mempty                  = GT_ 1
  mappend (GT_ a) (GT_ b) = GT_ (a * b)

instance Group GT where
  invert (GT_ a) = GT_ (recip a)
  pow (GT_ a) p  = GT_ (a ^^ p) -- temporary

instance Abelian GT

{-# INLINABLE mkGT #-}
mkGT :: Fp12 -> Maybe GT
mkGT a = case a ^ fr_modulus of
  1 -> Just (GT_ a)
  _ -> Nothing

{-# INLINABLE gt_powFr #-}
gt_powFr :: GT -> Fr -> GT
gt_powFr (GT_ a) p = GT_ (a ^ fromFr p) -- temporary
