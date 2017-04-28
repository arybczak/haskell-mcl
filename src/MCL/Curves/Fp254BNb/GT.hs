{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
module MCL.Curves.Fp254BNb.GT
  ( GT
  , mkGT
  , gt_powFr
  ) where

import Control.DeepSeq
import Data.Binary
import Data.Group
import GHC.Exts

import MCL.Curves.Fp254BNb.Fp12
import MCL.Curves.Fp254BNb.Fr
import qualified MCL.Internal.Prim as I

-- | Subgroup of Fp12* of @r@-th roots of unity.
newtype GT = GT_ { unGT :: Fp12 }
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

-- | Construct an element of GT from @a ∈ Fp12@. If @a@ is not an @r@-th root of
-- unity, no result is returned.
{-# INLINABLE mkGT #-}
mkGT :: Fp12 -> Maybe GT
mkGT a = case a ^ fr_modulus of
  1 -> Just (GT_ a)
  _ -> Nothing

-- | Raise the element of GT to the power @x ∈ Fr@. Note: it uses const-time
-- method, i.e. the time it takes to calculate the result depends only on the
-- bitlength of @x@.
{-# INLINABLE gt_powFr #-}
gt_powFr :: GT -> Fr -> GT
gt_powFr (GT_ a) p = GT_ (a ^ fromFr p) -- temporary

----------------------------------------

-- | Internal
instance I.Prim GT where
  prim_size _ = I.prim_size (proxy# :: Proxy# Fp12)
  prim_wrap   = \ba -> GT_ (I.prim_wrap ba)
  prim_unwrap = \gt -> I.prim_unwrap (unGT gt)
