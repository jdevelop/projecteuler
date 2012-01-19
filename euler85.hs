import Data.List

width x = (2+(x-1)) * x `div` 2

squares w h = (2*w+(h-1)*w) * h `div` 2

x `delta` y | x > y = x - y
            | otherwise = y - x

cmp a (_,_,_,s1) (_,_,_,s2) = (s1 `delta` a) `compare` (s2 `delta` a)

solveSqrt :: Integer -> Integer -> Integer -> [Double]
solveSqrt a b c | d < 0 = []
                | d == 0 = [mB / da]
                | otherwise = [(mB - sqrtD) / da, (mB + sqrtD) / da]
    where
        d = b^2 - 4*a*c
        sqrtD = sqrt $ fromIntegral d
        mB = fromIntegral (-b)
        da = fromIntegral (2*a)

findClosestH a x = minimumBy (cmp a) . concatMap f . filter (>0) $ solveSqrt w w ((-2)*a)
    where
        w = width x
        f h = let h1 = ceiling h
                  h2 = floor h
              in [(x,h1,x*h1, squares w h1),(x,h2,x*h2,squares w h2)]
                  
solve lim = minimumBy (cmp lim) $ map (findClosestH lim) [1..l]
    where
        l = floor . head . filter (>0) $ solveSqrt 1 1 ((-2)*lim)
