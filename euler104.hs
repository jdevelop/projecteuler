import Data.Array.Diff

fibs ::  [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

toDigits :: Integer -> [Integer]
toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

isPandigital ::  [Integer] -> Bool
isPandigital xs = let sample = array (1,9) $ zip [1..9] (repeat 0)
                  in f xs sample
    where
        f :: [Integer] -> Array Integer Int -> Bool
        f [] _ = True
        f (y:ys) a | y == 0 = False
                   | a ! y == 1 = False
                   | otherwise = f ys ( a // [(y,1)] )

isFibPandigital ::  Integer -> Bool
isFibPandigital x | x < 10^18 = False
                  | otherwise = let digits = toDigits x
                                in isPandigital (take 9 digits) && isPandigital (take 9 $ reverse digits)

main = print . snd . head . dropWhile (not . isFibPandigital .fst ) $ zip fibs [1..]
