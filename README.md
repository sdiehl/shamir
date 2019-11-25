<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

# Shamir Secret Sharing Scheme

This library provides an implementation of the Shamir's secret scheme as well as
its more general variant where many secrets can be shared together.

## Single secret

Shamir secret sharing is a cryptographic technique to split an arbitrary secret S
into N parts, of which at least K are required to reconstruct S.

Splitting a secret works by encoding the secret as the constant in a random
polynomial of K degree.

Using a fixed set of points we can reconstruct the secret using Lagrange
interpolation.

### Usage

Import language extensions and modules.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import Control.Monad.Random (getRandomR)
import Data.Field.Galois (Prime, rnd)
import Shamir (shareSecret, reconstructSecret)
import Shamir.Packed
```

```haskell
type Fq = Prime 21888242871839275222246405745257275088696311157297823662689037894645226208583

secret :: Fq
secret = 123456789

k :: Int
k = 3

n :: Int
n = 6

simpleExample :: IO ()
simpleExample = do
  putText $ "Parties: " <> show n
  putText $ "Threshold: " <> show k
  shares <- shareSecret secret k n

  -- Completeness
  if secret == reconstructSecret (take k shares)
    then putText $ "Success: A single secret can be reconstructed from a subset of shares bigger or equal than the threshold"
    else putText $ "Failure: A single secret cannot be reconstructed from a subset of shares bigger or equal than the threshold"

  -- Soundness
  if secret == reconstructSecret (take (k - 1) shares)
    then putText $ "Failure: A single secret can be reconstructed from a subset of shares smaller than the threshold"
    else putText $ "Success: A single secret cannot be reconstructed from a subset of shares smaller than the threshold"
```

## Multiple packed secrets

The Shamir packed secret sharing scheme is a generalized variant of Shamir's
scheme that allows to share an arbitrary number of secrets.
Lagrange interpolation is not a viable algorithm when the number of shares or
the number of secrets is high. To efficiently compute the interpolation we use
the Fast Fourier Transform algorithm. This FFT approach, on the other hand,
introduces several other constraints that are shown in the following example:

```haskell
type Prime746497 = Prime 746497

data Setup = Setup
  { threshold    :: Int -- Privacy threshold
  , numOfSecrets :: Int -- Number of secrets
  , numOfShares  :: Int -- Number of shares. numOfShares >= orderSmall
  , orderSmall   :: Int -- Number of coefficients to match privacy threshold
                        -- orderSmall = closestToPow2 (t + k + 1)
  }

setupParams :: IO Setup
setupParams = do
  t <- getRandomR (100, 200)
  k <- getRandomR (30, 50)
  let o = closestToPow2 (k + t + 1)
  s <- getRandomR (o , 500)
  pure $ Setup t k s o

-- Calculate m-th principal root of unity for powers of two.
calcOmega2 :: Int -> Int -> Prime746497
calcOmega2 threshold numOfSecrets =
  let m = closestToPow2 (numOfSecrets + threshold + 1)
  in findNthPrimitiveRootOfUnity getRootOfUnity2 m

-- Calculate n-th principal root of unity for powers of three.
calcOmega3 :: Int -> Prime746497
calcOmega3 numOfShares =
  let n = closestToPow3 (numOfShares + 1)
  in findNthPrimitiveRootOfUnity getRootOfUnity3 n

packedExample :: IO ()
packedExample = do
  -- Setup parameters
  Setup{..} <- setupParams

  -- Calculate primitive roots of unity
  let omega2 = calcOmega2 threshold numOfSecrets
      omega3 = calcOmega3 numOfShares

  -- Generate secrets
  secrets <- replicateM numOfSecrets (rnd @Prime746497)

  -- Generate shares
  shares <- shareSecrets omega2 omega3 secrets threshold numOfShares

  -- Completeness
  if secrets == reconstructSecrets omega2 omega3 (take orderSmall shares) numOfSecrets
    then putText $ "Success: Multiple packed secrets can be reconstructed from a subset of shares bigger or equal than the threshold"
    else putText $ "Failure: Multiple packed secrets cannot be reconstructed from a subset of shares bigger or equal than the threshold"

  -- Soundness
  if secrets /= reconstructSecrets omega2 omega3 (take (orderSmall - 1) shares) numOfSecrets
    then putText $ "Failure: Multiple packed secrets can be reconstructed from a subset of shares smaller than the threshold"
    else putText $ "Success: Multiple packed secrets cannot be reconstructed from a subset of shares smaller than the threshold"
```

We use markdown-unlit to ensure the code in the readme is valid. A `main`
function is required.

```haskell
main :: IO ()
main = do
  -- Single secret sharing scheme
  simpleExample

  -- Multiple packed secrets sharing scheme
  packedExample
```

## Disclaimer

This is experimental code meant for research-grade projects only. Please do not
use this code in production until it has matured significantly.

## License

```
Copyright (c) 2019 Adjoint Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
OR OTHER DEALINGS IN THE SOFTWARE.
```
