{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protolude

import Control.Monad.Random (getRandom)
import Data.Field.Galois (Prime, rnd, pow, PrimeField)
import Data.List ((!!))
import Data.Poly (VPoly, toPoly, eval, scale, deriv, unPoly)
import qualified Data.Vector as V
import Shamir (shareSecret, reconstructSecret)
import Shamir.FFT as FFT
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
--     , secret /= FFT.reconstructSecret (take (k-1) shares)
--     ]

type Prime433 = Prime 433

prop_find_nth_root_of_unity :: PrimeField f => (Int -> f) -> Int -> Property
prop_find_nth_root_of_unity getRootOfUnity n = monadicIO $ do
  let omega = FFT.findNthRootOfUnity getRootOfUnity n
  pure $ omega `pow` n == 1

test_packed_example_1 :: Assertion
test_packed_example_1 = do
  let secrets = [1,2,3] :: [Prime433]
      t = 3 -- threshold
      n = 8 -- #shares
      k = length secrets -- #secrets
      m = FFT.closestToPow2 (k + t + 1)
      -- `m`-th principal root of unity in Zp,
      -- where `m = secret_count + threshold + 1`
      -- must be a power of 2
      omega2 = FFT.findNthRootOfUnity FFT.getRootOfUnity2 m
  traceShowM omega2
      -- `n`-th principal root of unity in Zp, where
      -- `n = share_count + 1` must be a power of 3
  let n' = FFT.closestToPow3 (n + 1)
      omega3 = FFT.findNthRootOfUnity FFT.getRootOfUnity3 n'
  traceShowM omega3
  shares <- FFT.shareSecrets omega2 omega3 secrets t n
  traceShowM $ ("Shares: ", shares)
  secrets @=? FFT.reconstructSecrets omega2 omega3 shares k

test_fft2_example_1 :: Assertion
test_fft2_example_1 = do
  let omega2 = 354
      aCoeffs = [1, 2, 3, 4, 5, 6, 7, 8]
      aPoints = FFT.fft2 omega2 aCoeffs
  aPoints @=? ([36, -130, -287, 3, -4, 422, 279, -311] :: [Prime433])
  FFT.inverseDft2 omega2 aPoints @=? aCoeffs


test_fft3_example_1 :: Assertion
test_fft3_example_1 = do
  let omega3 = 150
      aCoeffs = [1, 2, 3, 4, 5, 6, 7, 8, 9]
      aPoints = FFT.fft3 omega3 aCoeffs
  aPoints @=? ([45, 404, 407, 266, 377, 47, 158, 17, 20] :: [Prime433])
  FFT.inverseDft3 omega3 aPoints @=? aCoeffs

type Prime746497 = Prime 746497

test_packed_example_2 :: Assertion
test_packed_example_2 = do
  let t = 155 -- threshold
      n = 728 -- #shares
      k = 100 -- #secrets
      omega2 = 95660     -- `m`-th principal root of unity in Zp,
                         -- where `m = secret_count + threshold + 1`
                         -- must be a power of 2
      omega3 = 610121    -- `n`-th principal root of unity in Zp, where
                         -- `n = share_count + 1` must be a power of 3.
  secrets <- replicateM 100 (rnd @Prime746497)
  shares <- FFT.shareSecrets omega2 omega3 secrets t n
  secrets @=? FFT.reconstructSecrets omega2 omega3 shares k

type Prime17 = Prime 17
test_newton_interpolation_general :: Assertion
test_newton_interpolation_general = do
  let coeffs = [1,2,3,4] :: [Prime17]
      poly = toPoly . V.fromList $ coeffs
      points = [5,6,7,8,9]
      values = eval poly <$> points

  [8,16,4,13,16] @=? values

  let recoveredPoly = newtonInterpolation points values
      recoveredValues = newtonEvaluate recoveredPoly <$> points

  NewtonPolynomial points [8, 8, -10, 4, 0] @=? recoveredPoly
  values @=? recoveredValues
  3 @=? newtonEvaluate recoveredPoly 10
  15 @=? newtonEvaluate recoveredPoly 11
  8 @=? newtonEvaluate recoveredPoly 12

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testGroup "FFT"
    [ testCase "FFT2 small example" test_fft2_example_1
    , testCase "FFT3 small example" test_fft3_example_1
    ]
  , testGroup "Newton Polynomial"
    [ testCase "General" $ test_newton_interpolation_general
    ]
  , testGroup "Roots of unity"
    [ testProperty "Find Nth root of unity. Powers of 2"
      $ prop_find_nth_root_of_unity (FFT.getRootOfUnity2 @Prime433)
    , testProperty "Find Nth root of unity. Powers of 3"
      $ prop_find_nth_root_of_unity (FFT.getRootOfUnity3 @Prime433)
    ]
  , testGroup "Shamir secret sharing"
    [ testProperty "Lagrange" prop_shamir_lagrange
    , testGroup "Packed"
      [ testCase "Small example" test_packed_example_1
      , testCase "Medium example" test_packed_example_2
      ]
    ]
  ]
