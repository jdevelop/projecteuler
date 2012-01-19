import Data.List

fnd :: Integer -> [Integer] -> [Integer]
fnd 1 _ = [1]
fnd x (p:ps) | s == p' = x^p:(fnd x ps)
             | s > p' = []
             | s < p' = fnd x ps
    where
        s = truncate ( logBase 10 (fromIntegral $ x^p) )
        p' = pred p

solve :: [Integer]
solve = concatMap (\x -> fnd x [1..]) [1..9]
