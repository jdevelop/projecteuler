import Data.List as DL
import Data.Array as DA

fact x = product [1..x]

cache = DA.array (0,9) $ (0,1):(DL.zip [1..9] (DL.map fact [1..9]))

toDigits x | x < 10 = [x]
           | otherwise = (x `mod` 10) : toDigits (x `div` 10)

sumF = sum . DL.map ( cache ! ) . toDigits


solve limit = drop 2 $ filter ( \x -> sumF x == x ) [1..limit]
