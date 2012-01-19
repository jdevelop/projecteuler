import Data.List

primes  = [2,3,5,7,11,13,17,19]

getPrimeMultipliers :: Int -> [Int]
getPrimeMultipliers 1 = []
getPrimeMultipliers x = let (a,b) = head [(y, x `div` y) | y <- primes, x `mod` y == 0]
                        in a : getPrimeMultipliers b

collapsePower :: [Int] -> [(Int,Int)]
collapsePower = map (\xs -> (head xs, length xs)) . group

findSmallestGCM :: [Int] -> Int
findSmallestGCM = foldr ( (*) . uncurry (^) ) 1 . map head . map (sortBy (\(_,a) (_,b) -> b `compare` a)) . groupBy (\(a,_) (b,_) -> a == b) . map (head) . group . sort . concatMap ( collapsePower . getPrimeMultipliers )
