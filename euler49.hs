import qualified Data.Set as DS
import Data.Function
import Data.List
import Data.Numbers.Primes

primesL l u = takeWhile (<u) $ dropWhile (<=l) primes

toDigits x | x < 10 =[x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

fromDigits xs = sum $ zipWith (\a b -> a*10^b) xs [0..]

findPrimes l = concatMap f . filter ((>2) . length) . groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ map (\x -> (fromDigits . sort $ toDigits x,x)) l
    where
        f (a@(_,x):b@(_,y):c@(_,z):rest) | y-x == z - y = [x,y,z]
                                         | otherwise = f (b:c:rest)
        f _ = []

solve l u = concatMap show . findPrimes $ primesL l u

main = print $ solve 999 10000
