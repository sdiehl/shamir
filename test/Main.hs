{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Protolude

import Data.Field.Galois (Prime)
import Data.List ((!!))
import Shamir (shareSecret, reconstructSecret)
import qualified Shamir.FFT as FFT
import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

type Fq = Prime 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

combinations :: Int -> [a] -> [[a]]
combinations m xs = combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

prop_shamir_lagrange :: Fq -> Property
prop_shamir_lagrange secret = monadicIO $ do
  n <- lift . generate $ arbitrary `suchThat` (> 2)
  k <- getPositive <$> (lift . generate $ arbitrary `suchThat` (< Positive n))
  shares <- lift $ shareSecret secret k n

  -- Combinations without replacement grow \binom{n}{k},
  -- so we take the first 100 combinations
  let fails = and $ (\i -> and (((/=) secret . reconstructSecret) <$> take 100 (combinations i shares))
                    ) <$> [0..k-1]
  let successes = and $ (\i -> and (((==) secret . reconstructSecret) <$> take 100 (combinations i shares))
                        ) <$> [k..n]
  pure $ and [fails, successes]

-- prop_shamir_FFT :: Fq -> Property
-- prop_shamir_FFT secret = monadicIO $ do
--   -- n must be a power of 2
--   n <- (^) 2 <$> (lift . generate $ arbitrary @Int `suchThat` (\x -> x < 5 && x > 2))
--   k <- getPositive <$> (lift . generate $ arbitrary `suchThat` (< Positive n))
--   shares <- lift $ FFT.shareSecret secret k n
--   pure $ and
--     [ secret == FFT.reconstructSecret shares
--     -- TODO: Enable this. Still not work for less than n shares
--     -- , secret == FFT.reconstructSecret getRootOfUnity (take k shares)
--     , secret /= FFT.reconstructSecret (take (k-1) shares)
--     ]

type SmallPrime = Prime 433

test_packed_small_example :: Assertion
test_packed_small_example = do
  let t = 4 -- threshold
      n = 8 -- #shares
      k = 3 -- #secrets
      omega2 = 354       -- `m`-th principal root of unity in Zp,
                         -- where `m = secret_count + threshold + 1`
                         -- must be a power of 2
      omega3 = 150       -- `n`-th principal root of unity in Zp, where
                         -- `n = share_count + 1` must be a power of 3.
      secrets = [1,2,3] :: [SmallPrime]
  shares <- FFT.shareSecrets omega2 omega3 secrets t n
  secrets @=? FFT.reconstructSecrets omega2 omega3 shares k

test_fft2_small_example :: Assertion
test_fft2_small_example = do
  let omega2 = 354
      aCoeffs = [1, 2, 3, 4, 5, 6, 7, 8]
      aPoints = FFT.fft2 omega2 aCoeffs
  aPoints @=? ([36, -130, -287, 3, -4, 422, 279, -311] :: [SmallPrime])
  FFT.inverseDft2 omega2 aPoints @=? aCoeffs


test_fft3_small_example :: Assertion
test_fft3_small_example = do
  let omega3 = 150
      aCoeffs = [1, 2, 3, 4, 5, 6, 7, 8, 9]
      aPoints = FFT.fft3 omega3 aCoeffs
  aPoints @=? ([45, 404, 407, 266, 377, 47, 158, 17, 20] :: [SmallPrime])
  FFT.inverseDft3 omega3 aPoints @=? aCoeffs

main :: IO ()
main = defaultMain $
  testGroup "Shamir secret sharing"
  [ testProperty "Lagrange" prop_shamir_lagrange
  , testGroup "FFT"
    [--  testProperty "Single secret" prop_shamir_FFT
      testCase "FFT2 small example" test_fft2_small_example
    , testCase "FFT3 small example" test_fft3_small_example
    ]
  , testGroup "Packed"
    [ testCase "Small example" test_packed_small_example
    ]
  ]
