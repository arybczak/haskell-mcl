-- | Elliptic curve offering 128 bits of security from Barreto-Naehrig
-- family. It uses the following parameters:
--
-- - @p  = 16798108731015832284940804142231733909889187121439069848933715426072753864723@
-- - @r  = 16798108731015832284940804142231733909759579603404752749028378864165570215949@
-- - @E  : Y² = X³ + 2, defined over 'Fp'.@
-- - @E' : Y² = X³ + 2/(1 + α), defined over 'Fp2'.@
--
module MCL.Curves.Fp254BNb
  ( module MCL.Curves.Fp254BNb.Fp
  , module MCL.Curves.Fp254BNb.Fp2
  , module MCL.Curves.Fp254BNb.Fp12
  , module MCL.Curves.Fp254BNb.Fr
  , module MCL.Curves.Fp254BNb.G1
  , module MCL.Curves.Fp254BNb.G2
  , module MCL.Curves.Fp254BNb.GT
  , module MCL.Curves.Fp254BNb.Pairing
  ) where

import MCL.Curves.Fp254BNb.Fp
import MCL.Curves.Fp254BNb.Fp12
import MCL.Curves.Fp254BNb.Fp2
import MCL.Curves.Fp254BNb.Fr
import MCL.Curves.Fp254BNb.G1
import MCL.Curves.Fp254BNb.G2
import MCL.Curves.Fp254BNb.GT
import MCL.Curves.Fp254BNb.Pairing
