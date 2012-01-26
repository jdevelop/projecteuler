import qualified Data.Set as DS

data TEquation a = Equation a a a deriving (Show, Eq)

instance (Ord a) => Ord (TEquation a)
    where
        (Equation a1 a2 a3) `compare` (Equation b1 b2 b3) = 
            decide . filter (/= EQ) . map (uncurry compare) $ zip [a1,a2,a3] [b1,b2,b3]
            where
                decide [] = EQ
                decide (x:_) = x

makeFraction s = let a0 = floor $ sqrt (fromIntegral s)
                 in solveEq a0 (Equation 0 1 a0)
    where
        solveEq a0 (Equation m d a) = 
            let m' = d*a-m
                d' = (s - m' * m') `div` d
                a' = floor $ (fromIntegral $ a0 + m') / ( fromIntegral d')
                eq = Equation m' d' a'
                in eq : solveEq a0 eq

findPeriod x | isPerfect x = 0
             | otherwise = length . go DS.empty $ makeFraction x
    where
        isPerfect x = let sqrtx = floor ( sqrt $ fromIntegral x) 
                      in sqrtx * sqrtx == x
        go memo (eq:eqs) | DS.member eq memo = []
                         | otherwise = eq : go (DS.insert eq memo) eqs


main = print . length . filter ( (/=0) . (`mod` 2) ) $ map findPeriod [1..10000]
