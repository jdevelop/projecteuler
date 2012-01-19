import Data.List

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

pairs x = (reverse . toPairs . toDigits $ x) ++ (repeat 0)
    where
        toPairs [] = []
        toPairs [x] = [x]
        toPairs (a:b:rest) = (b*10+a):toPairs rest

roots a bA = snd . head . dropWhile ((> bA) . fst) $ map (\x -> ((a*10+x)*x,x)) [9,8..0]

findSqrt x = let (a:rest) = pairs x
                 num = floor . sqrt . fromIntegral $ a
             in num : solve num (a-num^2) rest
    where
        solve num c (g:gs) = let bA=c*100+g
                                 a=num*2
                                 z = roots a bA
                             in z : solve (num*10+z) (bA-(a*10+z)*z) gs
                             
solve x = sum . map (sum . take 100 . findSqrt) $ filter (\a -> ((truncate $ sqrt (fromIntegral a)) ^ 2) /= a)[1..x]
