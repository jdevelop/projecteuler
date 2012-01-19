import Data.List

primes = 2 : 3 : [x | x <- [4 .. ], isPrime x ]

isPrime x = all (/=0) . map (x `mod`) $ takeWhile ((<=x) . (^2)) primes

fromDigits xs = sum $ zipWith (\x y -> x * (10^y)) xs [0 .. ]

solve = maximum . filter (isPrime) . concatMap (map fromDigits . permutations) $ inits [1..9]

main = print $ solve
