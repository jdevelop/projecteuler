numbersU 0 _ _ = []
numbersU len x xs = concatMap f [x..9]
    where
        f :: Int -> [[Int]]
        f a = let n = xs++[a]
              in n : (numbersU (pred len) a n)

numbersD 0 _ _ = []
numbersD len x xs = concatMap f [x,x-1..0]
    where
        f :: Int -> [[Int]]
        f a = let n = xs++[a]
              in n : (numbersD (pred len) a n)

solve l = numbersD l 9 []
