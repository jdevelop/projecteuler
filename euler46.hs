import Data.Numbers.Primes

satisfy x = or . map chk $ takeWhile (<=x) primes
    where
        chk p = let rem = x - p
                in f rem p
        f rem p | rem `mod` 2 == 1 = False
                | otherwise = let a = truncate . sqrt . fromIntegral $ rem `div` 2
                              in 2*a*a+p == x
        
solve = fst . head . dropWhile snd $ map (\x -> (x,satisfy x)) [3,5..]

main = print $ solve
