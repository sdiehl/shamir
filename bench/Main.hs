module Main where

import Protolude

import           Criterion.Main
import           Data.Pairing.BN254 (Fr)
import           Data.Poly          (VPoly, eval, toPoly)
import qualified Data.Vector        as V

import Shamir        (reconstructSecret, shareSecret)
import Shamir.Packed

secret :: Fr
secret = 12345

k :: Int
k = 5

n :: Int
n = 10

poly :: VPoly Fr
poly = toPoly $ V.fromList [1..100]

points :: [Fr]
points = [5..105]

values :: [Fr]
values = eval poly <$> points

interpolated :: NewtonPolynomial Fr
interpolated = newtonInterpolation points values

main :: IO ()
main = defaultMain
      [ bench "Share secret" $ nfIO (shareSecret secret k n)
      , env (shareSecret secret k n) $ \shares
        -> bench "Recover secret" $ nf reconstructSecret (take 5 shares)
      , bench "Newton polynomial" $
        nf (newtonInterpolation points) values
      , bench "Newton evaluation" $
        nf (fmap (newtonEvaluate interpolated)) points
      ]
