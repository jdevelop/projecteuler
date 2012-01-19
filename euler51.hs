import Data.List
import qualified Data.Set as DS
import System
import Control.Monad

primes = 2:3:[x | x<-[4..], isPrime x]

isPrime x = all (/=0) . map (x `mod`) . takeWhile ((<=x).(^2)) $ primes

primesCache = DS.fromList $ take 100000 primes

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

fromDigits xs = sum $ zipWith (\a b -> a*10^b) xs [0..]

findRepeating = map (head . fst) . filter ( (>1) . snd) . map f . group . sort
    where
        f x = (x,length x)

replaceNumber _ _ [] = []
replaceNumber a b (x:xs) | x == a = b:replaceNumber a b xs
                         | otherwise = x:replaceNumber a b xs

findFamily x = let digits = toDigits x
                   reps = findRepeating digits
               in max $ map (f digits) reps
    where
        f xs n = length . filter (flip DS.member primesCache) . map fromDigits . filter ((>0) . last) $ map (\x -> replaceNumber n x xs) [0..9]
        max [] = 0
        max xs = maximum xs

solve x = fst . head . filter ( (==x) . snd) $ map (\x -> (x, findFamily x)) primes

main = liftM (read . head) getArgs >>= print . solve
