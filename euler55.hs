reverseNum n =  sum $ zipWith (\a b -> a*10^b) (reverse $ r n) [0..]
    where r x | x < 10 = [x]
              | otherwise = x `mod` 10 : r ( x `div` 10)

isPalindrome x = x - reverseNum x == 0

isLychrel n x | n == 50 = True
              | n > 0 && isPalindrome x = False
              | otherwise = isLychrel (n+1) (x + reverseNum x)

main = print $ length $ filter (isLychrel 0) [1..10000]
