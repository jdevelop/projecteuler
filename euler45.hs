import Data.List
import System

solveSqrEq :: Double -> Double -> Integer -> [Integer]
solveSqrEq a b c | d == 0 = [round (b / (2 * a))]
                 | d < 0 = []
                 | otherwise = let (rt1,rt2) = (round $((-b) - d) / (2 * a), round $ ((-b) + d) / (2*a))
                               in  [rt1,rt2]
    where
        d = sqrt ( b^2 - 4 * a * ( fromIntegral c ) )

hexagonals ::  [Integer]
hexagonals = map (\x ->x * ( 2 * x - 1 )) [1 .. ]

triangle ::  Integral a => a -> a
triangle x = (x * (x + 1)) `div` 2

pentagonal ::  Integral a => a -> a
pentagonal x = (x * ( 3 * x - 1)) `div` 2

solve z = head . dropWhile ( <= z) $ filter (\x -> chk triangle fT x && chk pentagonal fP x) hexagonals
    where
        chk f fr p = not . null . filter ( == p) . map f . filter (>0) $ fr (-p)
        fP = solveSqrEq 1.5 (-0.5)
        fT = solveSqrEq 0.5 0.5

main = getArgs >>= print . solve . read . head
