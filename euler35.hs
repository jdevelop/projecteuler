{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Data.Bits
import System
import Data.Array.Diff as A

type Cache = DiffArray Int Int

isPrime n = all ( /= 0) . map (n `mod`) . takeWhile ( ( <= n) . (^2) ) $ primes

primes = 2 : 3 : [x | x <- [4 .. ], isPrime x ]

digits 0 _ = []
digits n b = n `mod` b : digits (n `div` b) b

undigits xs b = foldr (+) 0 $ zipWith (\x y-> x * b ^ y) (reverse xs) [0 .. ]

rotateDigits a@(x:xs) = a : rotateDigits (xs ++ [x])

rotations n base = let digs = digits n base
              in map ( flip undigits base ) . take (length digs) . rotateDigits . reverse $ digits n base

primesCache :: Int -> (Cache,[Int])
primesCache up = foldr (\x (c,p) -> (c // [(x,1)], x:p)) (cache,[]) $ takeWhile ( < up ) primes
    where
        cache = array (1,up) . zip [1..] $ replicate up 0

calculate :: Int -> Int -> [Int]
calculate up base = foldr ( checkPrimeRotations ) [] prms
    where
        (cache,prms) = primesCache up
        checkPrimeRotations x acc | all id . map ( ( == 1) . (cache ! ) ) $ rotations x base = x:acc
                                  | otherwise = acc

main = getArgs >>= putStrLn . show . length . flip calculate 10 . read . head
