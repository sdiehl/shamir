module Shamir.FFT
  ( -- shareSecret
  -- , reconstructSecret
    shareSecrets
  , reconstructSecrets
  , getRootOfUnity
  , getRootOfUnity3
  , fft2
  , fft3
  , inverseDft2
  , inverseDft3
  ) where

import Protolude
import Control.Error.Operator (assertM)
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (PrimeField(..), rnd, GaloisField, pow, char)
import qualified Data.List as List
import Data.Poly (VPoly, toPoly, eval)
import qualified Data.Vector as V

-- | Polynomial represented as a coefficient vector, little-endian
type CoeffVec f = [f]

-- | Discrete Fourier transform. Can be interpreted as some polynomial
-- evaluated at certain roots of unity. (In our case the length of
-- these lists will be a power of two.)
type DFT f = [f]

generateParameters :: Int -> Integer -> Integer -> (Integer, Integer, Integer)
generateParameters = notImplemented

-- | Calculate roots of unity of powers of 2
getRootOfUnity :: forall f. PrimeField f => Int -> f
getRootOfUnity k
  | 0 <= k     = 5^((char (witness :: f) - 1) `div` (2^k))
  | otherwise  = panic "getRootOfUnity: No primitive root for given power of 2"

getRootOfUnity3 :: forall f. PrimeField f => Int -> f
getRootOfUnity3 k
  | 0 <= k     = 5^((char (witness :: f) - 1) `div` (3^k))
  | otherwise  = panic "getRootOfUnity: No primitive root for given power of 3"


-- | Fast Fourier transformation.
fft2
  :: GaloisField k
  => k          -- ^ function that gives for input n the principal (3^n)-th root of unity
  -> CoeffVec k -- ^ length should be n
  -> DFT k
fft2 omega as
  = case length as of
      1 -> as
      n -> snd <$> List.sort combineResults
        where
          (bsCoeffs, csCoeffs) = split as
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

-- | Split a list into a list containing the odd-numbered and one with
-- the even-numbered elements.
split :: [a] -> ([a],[a])
split = foldr (\a (r1, r2) -> (a : r2, r1)) ([], [])

split3 :: [a] -> ([a], [a], [a])
split3 ls = foldr (\(i, ai) (bsi, csi, dsi)
                   -> case i `mod` 3 of
                        1 -> (ai : bsi, csi, dsi)
                        2 -> (bsi, ai : csi, dsi)
                        0 -> (bsi, csi, ai : dsi)
                  ) ([], [], []) (zip [1..] ls)

-- | Append minimal amount of zeroes until the list has a length which
-- is a power of two.
padToNearestPowerOfTwo :: Num f => [f] -> [f]
padToNearestPowerOfTwo [] = []
padToNearestPowerOfTwo xs = padToNearestPowerOfTwoOf (length xs) xs

-- | Given n, append zeroes until the list has length 2^n.
padToNearestPowerOfTwoOf
  :: Num f
  => Int -- ^ n
  -> [f] -- ^ list which should have length <= 2^n
  -> [f] -- ^ list which will have length 2^n
padToNearestPowerOfTwoOf i xs = xs ++ replicate padLength 0
  where
    padLength = nearestPowerOfTwo - length xs
    nearestPowerOfTwo = bit $ log2 i

-- | Calculate ceiling of log base 2 of an integer.
log2 :: Int -> Int
log2 x = floorLog + correction
  where
    floorLog = finiteBitSize x - 1 - countLeadingZeros x
    correction = if countTrailingZeros x < floorLog
                 then 1
                 else 0

log3 :: Int -> Int
log3 = floor . logBase 3.0 . fromIntegral

-- | Inverse discrete Fourier transformation, uses FFT.
inverseDft2 :: GaloisField k => k -> DFT k -> CoeffVec k
inverseDft2 primRootsUnity dft
  = let n = fromIntegral . length $ dft
    in (/ n) <$> fft2 (recip primRootsUnity) dft

-- | Inverse discrete Fourier transformation, uses FFT.
inverseDft3 :: GaloisField k => k -> DFT k -> CoeffVec k
inverseDft3 primRootsUnity dft
  = let n = fromIntegral . length $ dft
    in (/ n) <$> fft3 (recip primRootsUnity) dft

-- | Create a polynomial that goes through the given values.
interpolate :: GaloisField k => k -> [k] -> VPoly k
interpolate primRoots pts = toPoly . V.fromList $ inverseDft2 primRoots (padToNearestPowerOfTwo pts)

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
          orderSmall = t + k + 1
          orderLarge = n + 1

samplePolynomial :: (MonadRandom m, PrimeField f) => f -> [f] -> Int -> m [f]
samplePolynomial omega2 secrets t = do
  rndVs <- replicateM t rnd
  let values = 0 : secrets ++ rndVs
  -- Run backward FFT to recover polynomial in coefficient representation
  pure $ inverseDft2 omega2 values

reconstructSecrets
  :: PrimeField f
  => f
  -> f
  -> [f]            -- Shares
  -> Int            -- Number of secrets packed
  -> [f]
reconstructSecrets omega2 omega3 [] _ = []
reconstructSecrets omega2 omega3 shares k = secrets
  where
    largeValues = 0 : shares
    largeCoeffs = inverseDft3 omega3 largeValues
    smallCoeffs = take (length shares) largeCoeffs
    smallValues = fft2 omega2 smallCoeffs
    secrets = take k (drop 1 smallValues)

-- -- | Create shares from a secret
-- -- See https://mortendahl.github.io/2017/06/24/secret-sharing-part2/
-- shareSecret :: (MonadRandom m, PrimeField f) => f -> Int -> Int -> m [f]
-- shareSecret secret k n
--   | k <= 0 || n <= 0 = panic $ "k and n must be positive integers"
--   | k > n = panic $ "k cannot be greater than n"
--   | otherwise = do
--     rndVs <- replicateM (k-1) rnd
--     let coeffs = secret : rndVs ++ replicate (n + 1 - k) 0
--     pure $ fft2 getRootOfUnity coeffs

-- -- | Reconstruct secret using Fast Fourier Transform. Solve for f(0).
-- reconstructSecret :: PrimeField f => [f] -> f
-- reconstructSecret [] = 0
-- reconstructSecret shares = eval (interpolate getRootOfUnity shares) 0
