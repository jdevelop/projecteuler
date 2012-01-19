import Data.List
import qualified Data.Array as A
import qualified Data.Set as DS
import System
import Control.Monad

factorials = A.array (0,9) $ map (\x -> (x,fact x)) [0..9]
    where
        fact 0 = 1
        fact x = product [1..x]

fact x = factorials A.! x


toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

fromDigits xs = sum $ zipWith (\a b -> a*10^b) xs [0..]


loop m x = s 1 (DS.singleton x) x
    where
        s n c x | n > m = 0
                | otherwise = let num = sum . map fact $ toDigits x
                              in if DS.member num c then n else s (succ n) (DS.insert num c) num

main = do 
    l <- liftM ( read . head ) getArgs
    print . length . filter (==l) $ map (loop l) [2..999999]
