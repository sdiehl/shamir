{-|
Module      : Shamir.Packed
Description : Shamir packed secrets sharing scheme
Copyright   : (c) 2019 Adjoint Inc.
License     : MIT
Maintainer  : "Adjoint Inc (info@adjoint.io)"
-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Shamir.Packed
  ( shareSecrets
  , reconstructSecrets
  , findNthRootOfUnity
  , getRootOfUnity2
  , getRootOfUnity3
  , fft2
  , fft3
  , inverseDft2
  , inverseDft3
  , newtonInterpolation
  , newtonEvaluate
  , NewtonPolynomial(..)
  ) where

import Protolude hiding (quot)
import Control.Error.Operator (assertM)
import Control.Monad.Random (MonadRandom)
import Data.Euclidean (quot)
import Data.Field.Galois (PrimeField(..), rnd, GaloisField, pow, char)
import qualified Data.List as List
import Data.Poly (VPoly, toPoly, eval, scale, deriv)
import qualified Data.Vector as V

-- | Polynomial represented as a coefficient vector, little-endian
type CoeffVec f = [f]

-- | Discrete Fourier transform. Can be interpreted as some polynomial
-- evaluated at certain roots of unity. (In our case the length of
-- these lists will be a power of two.)
type DFT f = [f]

--------------------
-- Roots of unity --
--------------------

-- | Calculate roots of unity of powers of 2
getRootOfUnity2 :: forall f. PrimeField f => Int -> f
getRootOfUnity2 k
  | 0 <= k     = 5^((char (witness :: f) - 1) `div` (2^k))
  | otherwise  = panic "getRootOfUnity: No primitive root for given power of 2"

-- | Calculate roots of unity of powers of 3
getRootOfUnity3 :: forall f. PrimeField f => Int -> f
getRootOfUnity3 k
  | 0 <= k     = 5^((char (witness :: f) - 1) `div` (3^k))
  | otherwise  = panic "getRootOfUnity: No primitive root for given power of 3"

findNthRootOfUnity :: PrimeField f => (Int -> f) -> Int -> f
findNthRootOfUnity getRootOfUnity n = go 2
  where
    go k = let root = getRootOfUnity k
           in if root `pow` n == 1 then root else go (k + 1)


-------------------------------------
-- Newton Polynomial Interpolation --
-------------------------------------

data NewtonPolynomial f = NewtonPolynomial
  { npPoints :: [f]
  , npCoeffs :: [f]
  } deriving (Show, Eq)

newtonInterpolation :: PrimeField f => [f] -> [f] -> NewtonPolynomial f
newtonInterpolation points values = NewtonPolynomial points coeffs
  where
    initialStore = List.zip3 [0..] [0..] values
    coeffs = (\(_, _, c) -> c) <$> store
    g accStore i = take i accStore ++ ((indexLower, indexUpper, frac) : drop (i+1) accStore)
      where
        prevStoreElem = accStore List.!! (i-1)
        currStoreElem = accStore List.!! i
        (indexLower, _, coefLower) = prevStoreElem
        (_, indexUpper, coefUpper) = currStoreElem
        pointLower = points List.!! indexLower
        pointUpper = points List.!! indexUpper
        pointDiff = pointUpper - pointLower
        pointDiffInv = recip pointDiff
        coefDiff = coefUpper - coefLower
        frac = coefDiff * pointDiffInv
    f currStore j = foldl' g currStore (reverse [j..storeLen])
    store = foldl' f initialStore [1..storeLen]
    storeLen = length initialStore - 1

newtonEvaluate :: PrimeField f => NewtonPolynomial f -> f -> f
newtonEvaluate NewtonPolynomial{..} point
  = sum $ (\(p, c) -> c * p) <$> zip newtonPoints npCoeffs
  where
    f acc i = let diff = point - (npPoints List.!! i)
                  product = (acc List.!! i) * diff
              in acc ++ [product]
    newtonPoints = foldl' f [1] [0..length npPoints - 2]

---------
-- FFT --
---------

-- | Split a list into a list containing the odd-numbered and one with
-- the even-numbered elements.
split2 :: [a] -> ([a],[a])
split2 = foldr (\a (r1, r2) -> (a : r2, r1)) ([], [])

split3 :: [a] -> ([a], [a], [a])
split3 ls = foldr (\(i, ai) (bsi, csi, dsi)
                   -> case i `mod` 3 of
                        0 -> (ai : bsi, csi, dsi)
                        1 -> (bsi, ai : csi, dsi)
                        2 -> (bsi, csi, ai : dsi)
                  ) ([], [], []) (zip [0..] ls)

-- | Append minimal amount of zeroes until the list has a length which
-- is a power of two.
padToNearestPow2 :: forall f. Num f => [f] -> [f]
padToNearestPow2 [] = []
padToNearestPow2 xs = padToNearestPow2Of (length xs) xs
  where
    -- | Given n, append zeroes until the list has length 2^n.
    padToNearestPow2Of
      :: Num f
      => Int -- ^ n
      -> [f] -- ^ list which should have length <= 2^n
      -> [f] -- ^ list which will have length 2^n
    padToNearestPow2Of i xs = xs ++ replicate padLength 0
      where
        padLength = nearestPow2 - length xs
        nearestPow2 = bit $ log2 i

padToNearestPow3 :: forall f. Num f => [f] -> [f]
padToNearestPow3 [] = []
padToNearestPow3 xs = padToNearestPow3Of (length xs) xs
  where
    -- | Given n, append zeroes until the list has length 3^n.
    padToNearestPow3Of
      :: Num f
      => Int -- ^ n
      -> [f] -- ^ list which should have length <= 3^n
      -> [f] -- ^ list which will have length 3^n
    padToNearestPow3Of i xs = xs ++ replicate padLength 0
      where
        padLength = nearestPow3 - length xs
        nearestPow3 = 3 ^ (log3 i)

closestToPow2 :: Int -> Int
closestToPow2 = ((^) 2) . log2

closestToPow3 :: Int -> Int
closestToPow3 = ((^) 3) . log3

-- | Calculate ceiling of log base 2 of an integer.
log2 :: Int -> Int
log2 x = floorLog + correction
  where
    floorLog = finiteBitSize x - 1 - countLeadingZeros x
    correction = if countTrailingZeros x < floorLog
                 then 1
                 else 0

-- | Calculate ceiling of log base 3 of an integer.
log3 :: Int -> Int
log3 = ceiling . logBase 3.0 . fromIntegral


-- | Fast Fourier transformation.
fft2
  :: GaloisField k
  => k          -- ^ function that gives for input n the principal (2^n)-th root of unity
  -> CoeffVec k -- ^ length should be n
  -> DFT k
fft2 omega as
  = case length as of
      1 -> as
      n -> snd <$> List.sort combineResults
        where
          (bsCoeffs, csCoeffs) = split2 as
          lHalf = length bsCoeffs
          omegaSquared = omega `pow` 2
          bsValues = fft2 omegaSquared bsCoeffs
          csValues = fft2 omegaSquared csCoeffs
          combineResults = concat $ (\(i, bsi, csi)
               -> let j = i + lHalf
                      xi = omega `pow` i
                  in [ (i, bsi + xi * csi)
                     , (j, bsi - xi * csi)]
              ) <$> List.zip3 [0..] bsValues csValues

fft3
  :: GaloisField k
  => k          -- ^ function that gives for input n the principal (3^n)-th root of unity
  -> CoeffVec k -- ^ length should be n
  -> DFT k
fft3 omega as
  = case length as of
      1 -> as
      n -> snd <$> List.sort combineResults
        where
          (bsCoeffs, csCoeffs, dsCoeffs) = split3 as
          lThird = length bsCoeffs
          omegaCubed = omega `pow` 3
          bsValues = fft3 omegaCubed bsCoeffs
          csValues = fft3 omegaCubed csCoeffs
          dsValues = fft3 omegaCubed dsCoeffs
          combineResults = concat $ (\(i, bsi, csi, dsi)
               -> let j = i + lThird
                      k = i + 2 * lThird
                      xi = omega `pow` i
                      xj = omega `pow` j
                      xk = omega `pow` k
                  in [ (i, bsi + xi * csi + xi * xi * dsi)
                     , (j, bsi + xj * csi + xj * xj * dsi)
                     , (k, bsi + xk * csi + xk * xk * dsi)]
              ) <$> List.zip4 [0..] bsValues csValues dsValues

-- | Inverse discrete Fourier transformation, uses FFT.
inverseDft2 :: GaloisField k => k -> DFT k -> CoeffVec k
inverseDft2 primRootsUnity (padToNearestPow2 -> dft)
  = let n = fromIntegral . length $ dft
    in (/ n) <$> fft2 (recip primRootsUnity) dft

-- | Inverse discrete Fourier transformation, uses FFT.
inverseDft3 :: GaloisField k => k -> DFT k -> CoeffVec k
inverseDft3 primRootsUnity (padToNearestPow3 -> dft)
  = let n = fromIntegral . length $ dft
    in (/ n) <$> fft3 (recip primRootsUnity) dft

-- | Create a polynomial that goes through the given values.
interpolate2 :: GaloisField k => k -> [k] -> VPoly k
interpolate2 primRoots pts = toPoly . V.fromList $ inverseDft2 primRoots pts

-- | Create a polynomial that goes through the given values.
interpolate3 :: GaloisField k => k -> [k] -> VPoly k
interpolate3 primRoots pts = toPoly . V.fromList $ inverseDft3 primRoots pts

---------------------------
-- Packed secrets scheme --
---------------------------

samplePolynomial :: (MonadRandom m, PrimeField f) => f -> [f] -> Int -> m [f]
samplePolynomial omega2 secrets t = do
  rndVs <- replicateM t rnd
  let values = 0 : secrets ++ rndVs
  -- Run backward FFT to recover polynomial in coefficient representation
  pure $ inverseDft2 omega2 values

-- | Packed secrets
shareSecrets
  :: (MonadRandom m, PrimeField f)
  => f
  -> f
  -> [f]                            -- Secrets
  -> Int                            -- Threshold
  -> Int                            -- Number of shares
  -> m [f]
shareSecrets omega2 omega3 secrets t n
  | t <= 0 || n <= 0 = panic $ "k and n must be positive integers"
  | t > n = panic $ "k cannot be greater than n"
  | otherwise = do
      -- Sample polynomial
      poly <- samplePolynomial omega2 secrets t
      assertM (length poly == orderSmall) "Invalid number of small values"
      -- Extend polynomial
      let extendedPoly = poly ++ replicate (orderLarge - orderSmall) 0
      assertM (length extendedPoly == orderLarge) "Invalid number of large values"
      -- Evaluate polynomial to generate shares
      let shares = fft3 omega3 extendedPoly
      assertM (List.head shares == 0) "The first element of shares is not 0"

      pure $ drop 1 shares
        where
          k = length secrets
          orderSmall = closestToPow2 (t + k + 1)
          orderLarge = closestToPow3 (n + 1)

reconstructSecrets
  :: forall f. PrimeField f
  => f
  -> f
  -> [f]            -- Shares
  -> Int            -- Number of secrets packed
  -> [f]
reconstructSecrets omega2 omega3 [] _ = []
reconstructSecrets omega2 omega3 shares k
  = let points = 1 : ((omega3 `pow`) <$> [1..length shares])
        values = 0 : shares
        poly = newtonInterpolation points values
    in take k ((newtonEvaluate poly  . (omega2 `pow`)) <$> [1..])
