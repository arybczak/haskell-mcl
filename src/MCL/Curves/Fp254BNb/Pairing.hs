{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Pairing where

import MCL.Curves.Fp254BNb.Fp12
import MCL.Curves.Fp254BNb.G1
import MCL.Curves.Fp254BNb.G2
import MCL.Curves.Fp254BNb.GT
import MCL.Internal.Utils

pairing :: G1 -> G2 -> GT
pairing fp fq = unsafeOp0 . withG1 fp $ \p ->
                            withG2 fq $ \q ->
  GT_ <$> newFp12 (c_mcl_fp254bnb_pairing p q)

foreign import ccall unsafe "hs_mcl_fp254bnb_pairing"
  c_mcl_fp254bnb_pairing :: CC_G1 -> CC_G2 -> MC_Fp12 -> IO ()
