import Data.List

toBits 1 = [1]
toBits 0 = [0]
toBits x = x `mod` 2 : toBits (x `div` 2)

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

from base x = foldl (\acc (a,b) -> acc + a * (base^b) ) 0 $ zip x [0,1..]

solve limit = succ . sum . filter (\x -> x == from10 ( reverse (toDigits x ) ) ) . filter ( (<= limit ) . fromIntegral ) . 
                map from2 . concatMap (\x -> [x ++ reverse x,x ++ [1] ++ reverse x, x ++ [0] ++ reverse x]) . 
                concatMap (\x -> x:pad x bits) $ map toBits lst
    where
        from2 = from 2
        from10 = from 10
        lst = [1,3.. limit']
        limit' = 2 ^ bits
        bits = (round $ logBase 2 limit) `div` 2

pad x len | length x == len = []
          | otherwise = let newX = x ++ [0]
                        in newX : if length newX == len then [] else pad newX len
