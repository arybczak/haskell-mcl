{-# LANGUAGE UnliftedFFITypes #-}
module MCL.Curves.Fp254BNb.Pairing where

import MCL.Curves.Fp254BNb.Fp12
import MCL.Curves.Fp254BNb.G1
import MCL.Curves.Fp254BNb.G2
import MCL.Curves.Fp254BNb.GT
import MCL.Internal.Utils
import qualified MCL.Internal.Prim as I

pairing :: G1 -> G2 -> GT
pairing fp fq = unsafeOp0 . I.withPrim fp $ \p ->
                            I.withPrim fq $ \q ->
  I.newPrim_ (c_mcl_fp254bnb_pairing p q)

foreign import ccall unsafe "hs_mcl_fp254bnb_pairing"
  c_mcl_fp254bnb_pairing :: I.CC G1 -> I.CC G2 -> I.MC Fp12 -> IO ()
