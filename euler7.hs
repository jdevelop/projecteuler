module Main where

import Data.List
import Control.Monad
import System

isPrime :: [Integer] -> Integer -> Bool
isPrime _ 2 = True
isPrime _ 3 = True
isPrime (x:xs) y | x * x > y = True
                 | y `mod` x == 0 = False
                 | otherwise = isPrime xs y

primes :: Integer -> [Integer]
primes x = foldl ( nextPrime ) [2,3] [1 .. (x `div`6 + 1)]
    where
        nextPrime ps k = checkPrimeNext (6 * k + 1) $ checkPrimeNext ( 6 * k - 1) ps
        checkPrimeNext i ps | isPrime ps i = ps ++ [i]
                            | otherwise = ps

sumPrimes :: Integer -> Integer
sumPrimes x = sum . filter ( <x ) $ primes x

main = liftM (read . head) getArgs >>= putStrLn . show . sumPrimes
