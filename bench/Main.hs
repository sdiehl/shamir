module Main where

import Protolude

import Criterion.Main
import Data.Pairing.BN254 (Fr)
import Shamir (shareSecret, reconstructSecret)

secret :: Fr
secret = 12345

k :: Int
k = 5

n :: Int
n = 10

main :: IO ()
main = defaultMain
      [ bench "Share secret" $ nfIO (shareSecret secret k n)
      , env (shareSecret secret k n) $ \shares
        -> bench "Recover secret" $ nf reconstructSecret (take 5 shares)
      ]
