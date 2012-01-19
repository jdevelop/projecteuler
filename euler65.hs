import Data.Ratio
import Data.List

nums = 2:concatMap (\x -> [1,2*x,1]) [1..]

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

solve x = sum . toDigits . numerator . f . take x $ nums
    where
        f :: [Integer] -> Rational
        f (a:[]) = a % 1
        f (a:rest) = a % 1 + 1 / (f rest)
