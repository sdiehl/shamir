{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protolude

import Control.Monad.Random (getRandomR)
import Data.Field.Galois (Prime, rnd, pow, PrimeField)
import Data.List ((!!))
import Data.Poly (toPoly, eval)
import qualified Data.Vector as V
import Shamir (shareSecret, reconstructSecret)
import Shamir.Packed as Packed
import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

type Fq = Prime 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
type Prime433 = Prime 433
type Prime746497 = Prime 746497

combinations :: Int -> [a] -> [[a]]
combinations m xs = combsBySize xs !! m
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x next = zipWith (++) (map (map (x:)) ([]:next)) next

----------------------------
-- Fast Fourier Transform --
----------------------------

prop_find_nth_root_of_unity :: PrimeField f => (Int -> f) -> Int -> Property
prop_find_nth_root_of_unity getRootOfUnity n = monadicIO $ do
  let omega = findNthRootOfUnity getRootOfUnity n
  pure $ omega `pow` n == 1

test_fft2_example_1 :: Assertion
test_fft2_example_1 = do
  let omega2 = 354
      aCoeffs = [1, 2, 3, 4, 5, 6, 7, 8]
      aPoints = fft2 omega2 aCoeffs
  aPoints @=? ([36, -130, -287, 3, -4, 422, 279, -311] :: [Prime433])
  inverseDft2 omega2 aPoints @=? aCoeffs


test_fft3_example_1 :: Assertion
test_fft3_example_1 = do
  let omega3 = 150
      aCoeffs = [1, 2, 3, 4, 5, 6, 7, 8, 9]
      aPoints = fft3 omega3 aCoeffs
  aPoints @=? ([45, 404, 407, 266, 377, 47, 158, 17, 20] :: [Prime433])
  inverseDft3 omega3 aPoints @=? aCoeffs

-------------------------------------
-- Newton Polynomial Interpolation --
-------------------------------------

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

----------------------------------
-- Packed secret sharing scheme --
----------------------------------

-- Example 1

test_packed_example_1 :: Assertion
test_packed_example_1 = do
  let secrets = [1,2,3] :: [Prime433]
      t = 2 -- threshold
      s = 8 -- #shares
      k = length secrets -- #secrets
      -- `m`-th principal root of unity in Zp,
      -- where `m = secret_count + threshold + 1`
      -- must be a power of 2
      m = closestToPow2 (k + t + 1)
      omega2 = findNthPrimitiveRootOfUnity getRootOfUnity2 (closestToPow2 m)
      -- `n`-th principal root of unity in Zp, where
      -- `n = share_count + 1` must be a power of 3
      n = closestToPow3 (s + 1)
      omega3 = findNthPrimitiveRootOfUnity getRootOfUnity3 (closestToPow3 n)
  shares <- shareSecrets omega2 omega3 secrets t s
  l <- getRandomR (m, s)
  secrets @=? reconstructSecrets omega2 omega3 (take l shares) k

-- Example 2

test_packed_example_2 :: Assertion
test_packed_example_2 = do
  let t = 30 -- threshold
      s = 728 -- #shares
      k = 100 -- #secrets
      -- `m`-th principal root of unity in Zp,
      -- where `m = secret_count + threshold + 1`
      -- must be a power of 2
      m = closestToPow2 (k + t + 1)
      omega2 = findNthPrimitiveRootOfUnity getRootOfUnity2 (closestToPow2 m)
      -- `n`-th principal root of unity in Zp, where
      -- `n = share_count + 1` must be a power of 3
      n = closestToPow3 (s + 1)
      omega3 = findNthPrimitiveRootOfUnity getRootOfUnity3 (closestToPow3 n)
  secrets <- replicateM k (rnd @Prime746497)
  shares <- shareSecrets omega2 omega3 secrets t s
  l <- getRandomR (closestToPow2 (k + t + 1), s)
  secrets @=? reconstructSecrets omega2 omega3 (take l shares) k

--------------------------
-- Single secret scheme --
--------------------------

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

------------------------------------------------

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testGroup "FFT"
    [ testCase "FFT2" test_fft2_example_1
    , testCase "FFT3" test_fft3_example_1
    ]
  , testGroup "Newton Polynomial"
    [ testCase "General" $ test_newton_interpolation_general
    ]
  , testGroup "Roots of unity"
    [ testProperty "Find Nth root of unity. Powers of 2"
      $ prop_find_nth_root_of_unity (getRootOfUnity2 @Prime433)
    , testProperty "Find Nth root of unity. Powers of 3"
      $ prop_find_nth_root_of_unity (getRootOfUnity3 @Prime433)
    ]
  , testGroup "Shamir secret sharing"
    [ testProperty "Lagrange" prop_shamir_lagrange
    , testGroup "Packed"
      [ testCase "Example 1" test_packed_example_1
      , testCase "Example 2" test_packed_example_2
      ]
    ]
  ]
