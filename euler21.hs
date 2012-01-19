import Data.List

isPrime x = all (/=0) . map (x `mod`) . takeWhile ( (<=x) . (^2) )

primes=2:3:5:[x | x <- [6..], isPrime x primes]

primeDivisors 1 = []
primeDivisors x = let d = head . filter ((==0) . (x `mod`)) $ takeWhile (<= x) primes
                  in d : primeDivisors (x `div` d)

sumOfDivisors = product . map (+1) . map (length) . group . primeDivisors

isAmicable x | (x .&. 1) `xor` (x' .&. 1) == 1 = Nothing
             | x /= x' && x == x'' = Just (x,x')
             | otherwise = Nothing
    where
        x' = sum $ findDivisors x
        x'' = sum $ findDivisors x'
