module Shamir.FFT
  ( shareSecret
  , reconstructSecret
  ) where

import Protolude
import Control.Monad.Random (MonadRandom)
import Data.Field.Galois (PrimeField(..), rnd, GaloisField, pow)
import qualified Data.List as List
import Data.Poly (VPoly, toPoly, eval)
import qualified Data.Vector as V

-- | Polynomial represented as a coefficient vector, little-endian
type CoeffVec f = [f]

-- | Discrete Fourier transform. Can be interpreted as some polynomial
-- evaluated at certain roots of unity. (In our case the length of
-- these lists will be a power of two.)
type DFT f = [f]

-- | Fast Fourier transformation.
fft
  :: GaloisField k
  => (Int -> k) -- ^ function that gives for input n the principal (2^n)-th root of unity
  -> CoeffVec k -- ^ length should be n
  -> DFT k
fft omega_n as
  = case length as of
      1 -> as
      n ->
        let
          (as0, as1) = split as
          y0 = fft omega_n as0
          y1 = fft omega_n as1
          omegas = (pow (omega_n (log2 n))) <$> [0..n]
        in combine y0 y1 omegas
  where
    combine y0 y1 omegas
      = (\xs -> map fst xs ++ map snd xs)
      $ map (\(yk0, yk1, currentOmega) -> (yk0 + currentOmega * yk1, yk0 - currentOmega * yk1))
      $ List.zip3 y0 y1 omegas

-- | Split a list into a list containing the odd-numbered and one with
-- the even-numbered elements.
split :: [a] -> ([a],[a])
split = foldr (\a (r1, r2) -> (a : r2, r1)) ([], [])

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

-- | Inverse discrete Fourier transformation, uses FFT.
inverseDft :: GaloisField k => (Int -> k) -> DFT k -> CoeffVec k
inverseDft primRootsUnity dft
  = let n = fromIntegral . length $ dft
    in (/ n) <$> fft (recip . primRootsUnity) dft

-- | Create a polynomial that goes through the given values.
interpolate :: GaloisField k => (Int -> k) -> [k] -> VPoly k
interpolate primRoots pts = toPoly . V.fromList $ inverseDft primRoots (padToNearestPowerOfTwo pts)

-- | Create shares from a secret
-- See https://mortendahl.github.io/2017/06/24/secret-sharing-part2/
shareSecret :: (MonadRandom m, PrimeField f) => (Int -> f) -> f -> Int -> Int -> m [f]
shareSecret primRoots secret k n
  | k <= 0 || n <= 0 = panic $ "k and n must be positive integers"
  | k > n = panic $ "k cannot be greater than n"
  | otherwise = do
    rndVs <- replicateM (k-1) rnd
    let coeffs = secret : rndVs ++ replicate (n + 1 - k) 0
    pure $ fft primRoots coeffs

-- | Reconstruct secret using Fast Fourier Transform. Solve for f(0).
reconstructSecret :: PrimeField f => (Int -> f) -> [f] -> f
reconstructSecret primRoots shares = eval (interpolate primRoots shares) 0
