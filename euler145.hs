import System
import Control.Monad (liftM)

fromDigits x | x < 10 = [x]
             | otherwise = x `mod` 10 : fromDigits (x `div` 10)

isReversible :: [Int] -> Bool
isReversible xs = f 0 $ zip xs (reverse xs)
  where
    f _ [] = True
    f pred ((a,b):r) = let z = pred + a + b
                           (x,y) = divMod z 10
                       in if (y `mod` 2 == 1)
                          then f x r
                          else False

solve x = length $ filter (isReversible . fromDigits) $ filter ((/=0) . (`mod` 10)) [1..x]

main = getArgs >>= print . solve . read . head
