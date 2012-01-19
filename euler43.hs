import Data.List

perms ::  [[Integer]]
perms = filter ( (/=0) . head ) $ permutations [0..9]

check ::  Integral a => [a] -> [a] -> Bool
check [] _ = True
check (n:ns) (x:y:z:rest) | (x*100+y*10+z) `mod` n == 0 = check ns (y:z:rest)
                          | otherwise = False

solve ::  [[Integer]]
solve = filter ( check [2,3,5,7,11,13,17] . tail ) perms

fromDigits :: [Integer] -> Integer
fromDigits xs = sum $ zipWith (\a b -> a*10^b) xs [0..]

main = print . sum . map (fromDigits . reverse) $ solve
