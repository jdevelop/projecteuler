import Data.List as DL
import Data.Set as DS

primes = 2 : 3 : [x | x <- [4 .. ], isPrime x ]

isPrime x = all (/=0) . DL.map (x `mod`) $ takeWhile ((<=x) . (^2)) primes

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

fromDigits xs = sum $ zipWith (\x y -> x * (10^y)) xs [0 .. ]

truncatePrime x = DL.filter ( > 0) . DL.map (fromDigits) $ (init $ inits xs) ++ ( tail $ tails xs )
    where
        xs = toDigits x

truncatable = mapPrime DS.empty primes
    where
        allTr c x = all (flip DS.member c) $ truncatePrime x
        mapPrime c (x:xs) = if allTr c x then x : mapPrime (DS.insert x c) xs else mapPrime (DS.insert x c) xs

solve = sum $ take 11 $ DL.filter (>10) $ truncatable

main = print $ solve
