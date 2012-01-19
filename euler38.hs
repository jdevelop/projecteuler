import Data.List

items = permutations [1..9]

fromDigits x | x < 10 = [x]
             | otherwise = x `mod` 10 : fromDigits (x `div` 10)

toDigits xs = foldl (\acc (x,y) -> acc + x*10^y) 0 $ zip xs [0..]

canMake xs = concat $ concatMap (\x -> chk 1 x xs) c
    where 
        c = map (toDigits . reverse) $ map (flip take xs) [ 1 .. 5 ]
        chk _ _ [] = [xs]
        chk a x xs' = let sm = a * x
                          sm' = reverse $ fromDigits sm
                      in if sm' `isPrefixOf` xs' then chk (a+1) x (drop (length sm') xs') else []

solve = maximum $ map ( toDigits . reverse . canMake) items
    where
        f a x = a * x : f (a+1) x 

main = print $ solve
