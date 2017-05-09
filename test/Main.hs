{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Binary
import Data.Bits
import Data.Group
import Data.List
import Data.Monoid
import Data.Proxy
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ((.&.))

import MCL.Curves.Fp254BNb

main :: IO ()
main = defaultMain
  [ testGroup "Fr" $ let fr = Proxy :: Proxy Fr in
    [ testProperty "arithmetic"    $ testFieldOps fr
    , testProperty "construction"  $ fr_testFromTo
    , testProperty "square root"   $ fr_testSquareRoot
    , testProperty "serialization" $ testBinary fr
    ]
  , testGroup "Fp" $ let fp = Proxy :: Proxy Fp in
    [ testProperty "arithmetic"    $ testFieldOps fp
    , testProperty "construction"  $ fp_testFromTo
    , testProperty "square root"   $ fp_testSquareRoot
    , testProperty "serialization" $ testBinary fp
    ]
  , testGroup "Fp2" $ let fp2 = Proxy :: Proxy Fp2 in
    [ testProperty "arithmetic"    $ testFieldOps fp2
    , testProperty "construction"  $ fp2_testFromTo
    , testProperty "square root"   $ fp2_testSquareRoot
    , testProperty "serialization" $ testBinary fp2
    ]
  , testGroup "Fp12" $ let fp12 = Proxy :: Proxy Fp12 in
    [ testProperty "arithmetic"    $ testFieldOps fp12
    , testProperty "construction"  $ fp12_testFromTo
    , testProperty "serialization" $ testBinary fp12
    ]
  , testGroup "G1" $ let g1 = Proxy :: Proxy G1 in
    [ testProperty "arithmetic"    $ testGroupOps g1
    , testProperty "serialization" $ testBinary g1
    ]
  , testGroup "G2" $ let g2 = Proxy :: Proxy G2 in
    [ testProperty "arithmetic"    $ testGroupOps g2
    , testProperty "serialization" $ testBinary g2
    ]
  , testGroup "pairing"
    [ testProperty "bilinearity"   $ testPairing
    ]
  ]

----------------------------------------

arbitraryF :: (Integer -> fp) -> Gen fp
arbitraryF mkF = do
  w1 <- (0x1fffffff .&.) <$> arbitrary
  w2 <- arbitrary
  w3 <- arbitrary
  w4 <- arbitrary
  return . mkF $ foldl' assemble 0 [w1, w2, w3, w4]
  where
    assemble :: Integer -> Word64 -> Integer
    assemble acc w = acc `shiftL` 64 .|. fromIntegral w

instance Arbitrary Fr where
  arbitrary = arbitraryF mkFr

instance Arbitrary Fp where
  arbitrary = arbitraryF mkFp

instance Arbitrary Fp2 where
  arbitrary = mkFp2 <$> arbitrary <*> arbitrary

instance Arbitrary Fp12 where
  arbitrary = mkFp12 <$> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary G1 where
  arbitrary = mapToG1 <$> arbitrary

instance Arbitrary G2 where
  arbitrary = mapToG2 <$> arbitrary

----------------------------------------

testFieldOps
  :: forall proxy fp. (Arbitrary fp, Eq fp, Fractional fp, Show fp)
  => proxy fp
  -> Property
testFieldOps _ = conjoin
  [ property $ \(a::fp) b c -> (a + b) + c == a + (b + c)
  , property $ \(a::fp) b   -> a + b == b + a
  , property $ \(a::fp) b   -> a - b == -(b - a)
  , property $ \(a::fp) b c -> a * (b + c) == a * b + a * c
  , property $ \(a::fp) b c -> (a + b) * c == a * c + b * c
  , property $ \(a::fp)     -> a == -(-a)
  , property $ \(a::fp)     -> recip (recip a) == a
  ]

testGroupOps
  :: forall proxy g. (Arbitrary g, Eq g, Abelian g, Show g)
  => proxy g
  -> Property
testGroupOps _ = conjoin
  [ property $ \(a::g)            -> (a <> mempty) == a
  , property $ \(a::g)            -> (mempty <> a) == a
  , property $ \(a::g) b c        -> (a <> b) <> c == a <> (b <> c)
  , property $ \(a::g) b          -> a <> b == b <> a
  , property $ \(a::g) b          -> a <> invert b == invert (b <> invert a)
  , property $ \(a::g) b (c::Int) -> (a <> b) `pow` c == a `pow` c <> b `pow` c
  , property $ \(a::g)            -> a == invert (invert a)
  ]

testBinary
  :: forall proxy a. (Arbitrary a, Binary a, Eq a, Show a)
  => proxy a
  -> Property
testBinary _ = property $ \(a::a) -> decode (encode a) == a

testPairing :: Property
testPairing = conjoin
  [ property $ \p q r s -> pairing (p <> q) (r <> s) == pairing p r <> pairing p s
                                                     <> pairing q r <> pairing q s
  ]

----------------------------------------

fr_testFromTo :: Fr -> Bool
fr_testFromTo a = a == mkFr (fromFr a)

fp_testFromTo :: Fp -> Bool
fp_testFromTo a = a == mkFp (fromFp a)

fp2_testFromTo :: Fp2 -> Bool
fp2_testFromTo a = a == mkFp2 (fp2_c0 a) (fp2_c1 a)

fp12_testFromTo :: Fp12 -> Bool
fp12_testFromTo a = a == mkFp12 (fp12_c0 a) (fp12_c1 a) (fp12_c2 a)
                                (fp12_c3 a) (fp12_c4 a) (fp12_c5 a)

----------------------------------------

fr_testSquareRoot :: Fr -> Bool
fr_testSquareRoot a = t == Just a || t == Just (-a)
  where
    t = fr_squareRoot (a * a)

fp_testSquareRoot :: Fp -> Bool
fp_testSquareRoot a = t == Just a || t == Just (-a)
  where
    t = fp_squareRoot (a * a)

fp2_testSquareRoot :: Fp2 -> Bool
fp2_testSquareRoot a = t == Just a || t == Just (-a)
  where
    t = fp2_squareRoot (a * a)
