{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Protolude

import Data.Field.Galois (Prime)
import Data.List ((!!))
import Shamir (shareSecret, reconstructSecret)
import qualified Shamir.FFT as FFT (shareSecret, reconstructSecret)
import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty
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

prop_shamir_FFT :: Fq -> Property
prop_shamir_FFT secret = monadicIO $ do
  -- n must be a power of 2
  n <- (^) 2 <$> (lift . generate $ arbitrary @Int `suchThat` (\x -> x < 5 && x > 2))
  k <- getPositive <$> (lift . generate $ arbitrary `suchThat` (< Positive n))
  shares <- lift $ FFT.shareSecret secret k n
  pure $ and
    [ secret == FFT.reconstructSecret shares
    -- TODO: Enable this. Still not work for less than n shares
    -- , secret == FFT.reconstructSecret getRootOfUnity (take k shares)
    , secret /= FFT.reconstructSecret (take (k-1) shares)
    ]

main :: IO ()
main = defaultMain $
  testGroup "Shamir secret sharing"
  [ testProperty "Lagrange" prop_shamir_lagrange
  , testProperty "FFT" prop_shamir_FFT
  ]
