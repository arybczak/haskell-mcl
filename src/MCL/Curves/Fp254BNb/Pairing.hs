{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Pairing where

import MCL.Curves.Fp254BNb.Fp12
import MCL.Curves.Fp254BNb.G1
import MCL.Curves.Fp254BNb.G2
import MCL.Curves.Fp254BNb.GT
import qualified MCL.Internal.Prim as I

pairing :: G1 -> G2 -> GT
pairing = I.unsafeOp2_ c_mcl_fp254bnb_pairing

foreign import ccall unsafe "hs_mcl_fp254bnb_pairing"
  c_mcl_fp254bnb_pairing :: I.CC G1 -> I.CC G2 -> I.MC Fp12 -> IO ()
