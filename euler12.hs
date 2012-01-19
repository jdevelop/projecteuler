import Data.List

isPrime x = all (/=0) . map (x `mod`) . takeWhile ( (<=x) . (^2) )

primes=2:3:5:[x | x <- [6..], isPrime x primes]

primeDivisors 1 = []
primeDivisors x = let d = head . filter ((==0) . (x `mod`)) $ takeWhile (<= x) primes
                  in d : primeDivisors (x `div` d)

numberOfDivisors = product . map (+1) . map (length) . group . primeDivisors

triangles = 1 : 3 : zipWith (+) ( tail [2..] ) (tail triangles)

findNumber x = head . snd $ span ( (< x) . numberOfDivisors ) triangles
