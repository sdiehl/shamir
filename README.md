<p align="center">
<a href="https://www.adjoint.io">
  <img width="250" src="./.assets/adjoint.png" alt="Adjoint Logo" />
</a>
</p>

# Shamir

Shamir secret sharing is a cryptographic technique to split an arbitrary secret S
into N parts, of which at least K are required to reconstruct S.

Splitting a secret works by encoding the secret as the constant in a random
polynomial of K degree.

Using a fixed set of points we can reconstruct the secret using Lagrange
interpolation.

## Usage

Import language extensions and modules:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import Control.Monad.Random (getRandomR)
import Data.Field.Galois (Prime, rnd)
import Shamir (shareSecret, reconstructSecret)
import Shamir.Packed
```


### Single secret sharing

Example of sharing and reconstructing a secret:

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
  putText $ "Secret reconstructed from minimum subset of shares: "
    <> (show $ secret == reconstructSecret (take k shares))
  putText $ "Secret reconstructed from less than minimum subset of shares: "
    <> (show $ secret == reconstructSecret (take (k - 1) shares))
```

### Packed secrets

Generalized variant of Shamir's scheme to share an arbitrary number of secrets
efficiently using the Fast Fourier Transform.

```haskell
type Prime746497 = Prime 746497

packedExample :: IO ()
packedExample = do
  t <- getRandomR (100, 200) -- threshold
  k <- getRandomR (30, 50)  -- #secrets
  let m = closestToPow2 (k + t + 1)  -- m-th principal root of unity.
                                     -- Must be a power of two.
      omega2 = findNthPrimitiveRootOfUnity getRootOfUnity2 m
  s <- getRandomR (m, 500) -- #shares

  secrets <- replicateM k (rnd @Prime746497)
  let n = closestToPow3 (s + 1)      -- n-th principal root of unity.
                                     -- Must be a power of three.
      omega3 = findNthPrimitiveRootOfUnity getRootOfUnity3 n

  shares <- shareSecrets omega2 omega3 secrets t s
  l <- getRandomR (closestToPow2 (k + t + 1), s)
  putText $ "Packed secrets reconstructed from more than minimum subset of
    shares: "
    <> (show $ secrets == reconstructSecrets omega2 omega3 (take l shares) k)
```

```haskell
main :: IO ()
main = do
  simpleExample
  packedExample
```

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
