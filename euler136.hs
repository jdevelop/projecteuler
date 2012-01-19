import Data.List

solveSqrEQ a b c | d == 0 = [(-b) `div` da]
                 | d < 0 = []
                 | otherwise = [((-b) - sqrtD) `div` da,((-b) + sqrtD) `div` da]
    where
        da = 2*a
        d = b^2-4*a*c
        sqrtD = truncate . sqrt $ fromIntegral d

solveEq n z = map (\x ->4*n- (3*x^2-2*x*z-5*z^2)) . filter (>0) $ solveSqrEQ 3 ((-2)*z) (-(5*z^2+4*n))
