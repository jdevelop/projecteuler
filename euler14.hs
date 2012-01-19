module Main where

import Data.Map as DM
import Data.Int
import Data.Ord (comparing)
import Data.List (maximumBy, foldl')
import System
import Control.Monad

type Cache = DM.Map Int64 Int64

cache :: Cache
cache = DM.singleton 1 1

findMaxSequence n = extract $ foldr takeMax (0,0,cache) [n, n-1 .. 2]
    where
        extract (x,_,_) = x
        takeMax x s@(a,maxX,cs) = go $ buildMap x cs
            where
                go (maxX',cs') | maxX < maxX' = (x, maxX', cs')
                               | otherwise = (a,maxX,cs')

buildMap :: Int64 -> Cache -> (Int64, Cache)
buildMap x cs = go $ DM.lookup x cs
    where
        go Nothing = let (count,cs') = buildMap (nextX x) cs
                     in (1+count, DM.insert x (1 + count) cs')
        go (Just y) = (y,cs)

nextX x | x `mod` 2 == 0 = x `div` 2
        | otherwise = 3 * x + 1

nextXs 1 = [1]
nextXs n = n : nextXs ( nextX n )

main = liftM ( read . head ) getArgs >>= putStrLn . show . findMaxSequence
