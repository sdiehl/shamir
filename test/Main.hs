module Main where

import Protolude

import Data.List ((!!))
import Data.Pairing.BN254 (Fr)
import Shamir (shareSecret, reconstructSecret)
import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty
import Test.Tasty.QuickCheck

prop_shamir_sharing :: Fr -> Property
prop_shamir_sharing secret = monadicIO $ do
  n <- getPositive <$> (lift . generate $ arbitrary `suchThat` (> Positive 2))
  k <- getPositive <$> (lift . generate $ arbitrary `suchThat` (< Positive n))
  shares <- lift $ shareSecret secret k n

  -- Combinations without replacement grow \binom{n}{k},
  -- so we take the first 100 combinations
  let fails = and $ (\i -> and (((/=) secret . reconstructSecret) <$> take 100 (combinations i shares))
                    ) <$> [0..k-1]
  let successes = and $ (\i -> and (((==) secret . reconstructSecret) <$> take 100 (combinations i shares))
                        ) <$> [k..n]
  pure $ and [fails, successes]
  where
    combinations :: Int -> [a] -> [[a]]
    combinations m xs = combsBySize xs !! m
      where
        combsBySize = foldr f ([[]] : repeat [])
        f x next = zipWith (++) (map (map (x:)) ([]:next)) next

main :: IO ()
main = defaultMain $
  testProperty "Shamir secret sharing" prop_shamir_sharing
