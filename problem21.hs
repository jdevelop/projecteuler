import Data.List
import Data.Bits

isPrime x = all (/=0) . map (x `mod`) . takeWhile ( (<=x) . (^2) )

primes=2:3:5:[x | x <- [6..], isPrime x primes]


primeDivisors 1 = []
primeDivisors x = let d = head . filter ((==0) . (x `mod`)) $ takeWhile ( <= x) primes
                  in d : primeDivisors (x `div` d)

sumOfDivisors n = (foldr (*) 1 . map f . group . primeDivisors $ n) - n
    where
        f a@(x:_) = (x ^ (length a + 1) -1) `div` (x-1)

isAmicable :: Integer -> Bool
isAmicable x | (x .&. 1) `xor` (x' .&. 1) == 1 = False
             | otherwise = x /= x' && x == x''
    where
        x' = sumOfDivisors x
        x'' = sumOfDivisors x'

findSum n = sum $ map snd . filter ( fst ) $ zip (map isAmicable [2..n]) [2..n]
