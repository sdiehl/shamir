# Shamir

Shamir secret sharing is a cryptographic technique to split an arbitrary secret S
into N parts, of which at least K are required to reconstruct S.

Splitting a secret works by encoding the secret as the constant in a random
polynomial of K degree.

Using a fixed set of points we can reconstruct the secret using Lagrange
interpolation.

## Usage

A simple example of sharing and reconstructing a secret:

```haskell
import Protolude
import Data.Field.Galois (Prime)
import Shamir (shareSecret, reconstructSecret)

type Fq = Prime 21888242871839275222246405745257275088696311157297823662689037894645226208583

secret :: Fq
secret = 123456789

k :: Int
k = 3

n :: Int
n = 6

main :: IO ()
main = do
  putText $ "Parties: " <> show n
  putText $ "Threshold: " <> show k
  shares <- shareSecret secret k n
  putText $ "Secret reconstructed from minimum subset of shares: "
    <> (show $ secret == reconstructSecret (take k shares))
  putText $ "Secret reconstructed from minimum subset of shares: "
    <> (show $ secret == reconstructSecret (take (k - 1) shares))
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
