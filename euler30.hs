import Data.List

toDigits :: Int -> [Int]
toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

numbers = filter ( \(x,y) -> x == y ) . map (\(f,s) -> (sum $ map (^5) f,s)) $ zip (map (reverse . toDigits) [1..limit]) [1..limit]
    where 
        limit = 7*(9^5)

solution = sum . map fst . tail $ numbers
