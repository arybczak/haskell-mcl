{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Binary
import Data.ByteString.Lazy (ByteString)
import Data.Group
import Data.Maybe

import MCL.Curves.Fp254BNb

main :: IO ()
main = defaultMain
  [ bgroup "Fp"
    [ benchFpArith fp_a fp_b
    , bench "hash_to"       $ nf hashToFp "54o6vyua2984v357b35n63"
    , bgroup "from_integer"
      [ bench "small"       $ nf mkFp small_integer
      , bench "large"       $ nf mkFp large_integer
      ]
    , bench "eq"            $ nf (uncurry (==)) (fp_a, fp_b)
    , bench "to_integer"    $ nf fromFp fp_a
    , bench "is_zero"       $ nf fp_isZero fp_a
    , bench "sqrt"          $ nf fp_squareRoot fp_a
    , bench "show"          $ nf show fp_a
    , benchBinary fp_a
    ]
  , bgroup "Fp2"
    [ benchFpArith fp2_a fp2_b
    , bench "from_base" $ nf (uncurry mkFp2) (fp_a, fp_b)
    , bench "eq"        $ nf (uncurry (==)) (fp2_a, fp2_b)
    , bench "is_zero"   $ nf fp_isZero fp_a
    , bench "sqrt"      $ nf fp2_squareRoot fp2_a
    , bench "show"      $ nf show fp2_a
    , benchBinary fp2_a
    ]
  , bgroup "Fp12"
    [ benchFpArith fp12_a fp12_b
    , bench "eq"        $ nf (uncurry (==)) (fp12_a, fp12_b)
    , bench "is_zero"   $ nf fp12_isZero fp12_a
    , bench "show"      $ nf show fp12_a
    , benchBinary fp12_a
    ]
  , bgroup "Fr"
    [ benchFpArith fr_a fr_b
    , bench "hash_to"       $ nf hashToFr "6mn8o8rmn634wcxq354x31"
    , bgroup "from_integer"
      [ bench "small"       $ nf mkFr small_integer
      , bench "large"       $ nf mkFr large_integer
      ]
    , bench "eq"            $ nf (uncurry (==)) (fr_a, fr_b)
    , bench "to_integer"    $ nf fromFr fr_a
    , bench "is_zero"       $ nf fr_isZero fr_a
    , bench "show"          $ nf show fr_a
    , benchBinary fr_a
    ]
  , bgroup "G1"
    [ benchGroupArith g1_powFr g1_p g1_q
    , bench "construct" $ nf (uncurry mkG1) (g1_p_x, g1_p_y)
    , bench "map_to"    $ nf mapToG1 fp_a
    , bench "eq"        $ nf (uncurry (==)) (g1_p, g1_q)
    , bench "is_zero"   $ nf g1_isZero g1_p
    , bench "affine"    $ nf g1_affineCoords g1_p
    , bench "show"      $ nf show g1_p
    , benchBinary g1_p
    ]
  , bgroup "G2"
    [ benchGroupArith g2_powFr g2_p g2_q
    , bench "construct" $ nf (uncurry mkG2) (g2_p_x, g2_p_y)
    , bench "map_to"    $ nf mapToG2 fp2_a
    , bench "eq"        $ nf (uncurry (==)) (g2_p, g2_q)
    , bench "is_zero"   $ nf g2_isZero g2_p
    , bench "affine"    $ nf g2_affineCoords g2_p
    , bench "show"      $ nf show g2_p
    , benchBinary g2_p
    ]
  , bgroup "GT"
    [ bench "pow"        $ nf (uncurry pow) (gt_a, large_integer)
    , bench "pow_native" $ nf (uncurry gt_powFr) (gt_a, large_integer_fr)
    ]
  , bgroup "pairing"
    [ bench "compute1" $ nf (uncurry pairing) (g1_p, g2_q)
    , bench "compute2" $ nf (uncurry pairing) (g1_q, g2_p)
    ]
  ]

----------------------------------------

benchFpArith :: (Fractional a, NFData a) => a -> a -> Benchmark
benchFpArith a b = bgroup "arith"
  [ bench "add"           $ nf (uncurry (+)) (a, b)
  , bench "subtract"      $ nf (uncurry (-)) (a, b)
  , bench "multiply"      $ nf (uncurry (*)) (a, b)
  , bench "negate"        $ nf negate a
  , bench "invert"        $ nf recip a
  ]

benchGroupArith :: (Group g, NFData g) => (g -> Fr -> g) -> g -> g -> Benchmark
benchGroupArith fpow p q = bgroup "arith"
  [ bench "add"       $ nf (uncurry mappend) (p, q)
  , bench "invert"    $ nf invert p
  , bgroup "mul"
    [ bench "small"   $ nf (uncurry pow) (p, small_integer)
    , bench "large"   $ nf (uncurry pow) (p, large_integer)
    , bench "native"  $ nf (uncurry fpow) (p, large_integer_fr)
    ]
  ]

benchBinary :: forall a. (Binary a, NFData a) => a -> Benchmark
benchBinary a = bgroup "binary"
  [ bench "put" $ nf encode a
  , bench "get" $ nf (decode :: ByteString -> a) (encode a)
  ]

----------------------------------------

fr_a, fr_b :: Fr
fr_a = hashToFr "a"
fr_b = hashToFr "b"

fp_a, fp_b :: Fp
fp_a = hashToFp "a"
fp_b = hashToFp "b"

fp2_a, fp2_b :: Fp2
fp2_a = mkFp2 (hashToFp "a") (hashToFp "b")
fp2_b = mkFp2 (hashToFp "c") (hashToFp "d")

fp12_a, fp12_b :: Fp12
fp12_a = mkFp12 (mkFp2 (hashToFp "a") (hashToFp "b"))
                (mkFp2 (hashToFp "c") (hashToFp "d"))
                (mkFp2 (hashToFp "e") (hashToFp "f"))
                (mkFp2 (hashToFp "g") (hashToFp "h"))
                (mkFp2 (hashToFp "i") (hashToFp "j"))
                (mkFp2 (hashToFp "k") (hashToFp "l"))

fp12_b = mkFp12 (mkFp2 (hashToFp "m") (hashToFp "n"))
                (mkFp2 (hashToFp "o") (hashToFp "p"))
                (mkFp2 (hashToFp "q") (hashToFp "r"))
                (mkFp2 (hashToFp "s") (hashToFp "t"))
                (mkFp2 (hashToFp "u") (hashToFp "v"))
                (mkFp2 (hashToFp "w") (hashToFp "x"))

----------------------------------------

g1_p, g1_q :: G1
g1_p = mapToG1 fp_a
g1_q = mapToG1 fp_b

g1_p_x, g1_p_y :: Fp
(g1_p_x, g1_p_y) = fromJust $ g1_affineCoords g1_p

----------------------------------------

g2_p, g2_q :: G2
g2_p = mapToG2 fp2_a
g2_q = mapToG2 fp2_b

g2_p_x, g2_p_y :: Fp2
(g2_p_x, g2_p_y) = fromJust $ g2_affineCoords g2_p

gt_a :: GT
gt_a = pairing g1_p g2_q

----------------------------------------

small_integer, large_integer :: Integer
small_integer = 42
large_integer = fr_modulus `quot` 2

large_integer_fr :: Fr
large_integer_fr = mkFr large_integer
