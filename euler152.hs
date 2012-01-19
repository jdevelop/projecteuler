import Data.List
import Data.Array

type State = (Integer, [Integer], Integer)

probeMatch :: [Integer] -> Integer -> Integer -> State
probeMatch denums prevDenum num = let
    sqrNum = num ^ 2
    newDenum = prevDenum * sqrNum
    newDenums = map (sqrNum *) denums
    newNum = 2 * (prevDenum + sum newDenums)
    in (newDenum, prevDenum:newDenums, newNum)

diffs (a:b:c:src) =
    let (denums,_,nums) = foldl probeMatch' initialState src
    in nums `compare` denums
    where
        denum = a^2 * b^2
        initialState = probeMatch [a^2,b^2] denum c
        probeMatch' (newDenum, newDenums, newLeft) = probeMatch newDenums newDenum

solve :: [Integer] -> [[Integer]]
solve src = 
    reverse $ foldl ( go ) [] [(i,j,k) | i <- [0..len], j <- [i+1..len], k <- [j+1..len]]
    where
        len = fromIntegral $ length src - 1
        arr = listArray (0,len) src
        go :: [[Integer]] -> (Integer, Integer, Integer) -> [[Integer]]
        go result (i,j,k) = let
            a = arr ! i
            b = arr ! j
            c = arr ! k
            denum = a^2 * b^2
            initialState = probeMatch [a^2,b^2] denum c
            in ( findANumberW [a,b,c] (k+1) initialState ) : result
        findANumberW :: [Integer] -> Integer -> State -> [Integer]
        findANumberW results idx state@(denum,denums,num) 
            | idx <= len = findANumber results idx state
            | denum == num = results
            | otherwise = []
        findANumber :: [Integer] -> Integer -> State -> [Integer]
        findANumber results idx (denum, denums, num) 
            | denum == num = results
            | otherwise = let
                c = arr ! idx
                newResults = c:results
                newState = probeMatch denums denum c
                in findANumberW newResults (idx + 1) newState
