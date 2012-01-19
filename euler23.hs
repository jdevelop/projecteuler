import Data.List as DL
import Data.Set as DS

primes = 2 : 3 : 5 : [x | x <- [6..], isPrime x ]

isPrime x = all ( /=0 ) . DL.map (x `mod` ) . takeWhile (( <=x) . (^2)) $ primes

divisors x = divisors' x primes
    where
        divisors' n pp@(p:ps) | n == 1 = []
                              | n `mod` p == 0 = p : divisors' (n `div` p) pp
                              | otherwise = divisors' n ps

sumDivisors x = (product . DL.map (\(b,p) -> (b^(p+1) - 1) `div` (b - 1)) . DL.map (\x->(head x, length x)) . group . divisors $ x ) - x

abundants = DS.fromList $ DL.filter ( uncurry (<)) $ DL.map (\x -> (x,sumDivisors x)) [2 .. 28123]
