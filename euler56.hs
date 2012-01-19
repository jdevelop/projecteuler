import Data.List

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

solve = maximum . map (sum . toDigits) $ [a^b | a <-[1..100], b<-[1..100]]
