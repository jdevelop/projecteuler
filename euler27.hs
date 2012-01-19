import Data.List as DL
import Data.Set as DS
import System

primes = 2 : 3 : 5 : [x | x <- [6 .. ], isPrime x ]

isPrime x = all ( /= 0) . DL.map ( (x `mod`) ) . DL.takeWhile ( (<=x) . (^2) ) $ primes

producer a b = length $ takeWhile (isPrime) [n^2 + a * n + b | n <- [0 .. ] ]

calculate :: Int -> Int -> (Int,Int,Int)
calculate a b = maximumBy (\x y -> compare (third x) (third y)) [(a,b,len) | a <- [-a, (-a)+1 .. a], b <- bs, let len = producer a b ]
    where
        third (_,_,x) = x
        bsp = takeWhile ( <= 1000 ) primes
        bs = (reverse (DL.map ( * (-1) ) bsp) ++ bsp) :: [Int]


main = getArgs >>= print . calculate'
    where
        calculate' (a:b:_) = let (m,n,_) = calculate (read a) (read b)
                             in m * n
