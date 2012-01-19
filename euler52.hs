import Data.List

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits ( x `div` 10 )

fromDigits x = sum $ zipWith (\a b -> a*10^b) x [0 .. ]

calculate = head $ filter comp [1 .. ]
    where
        comp x = chk $ map ( fromDigits . sort . toDigits . ( * x) ) [2..6]
        chk (x:xs) = all ( == x) xs
