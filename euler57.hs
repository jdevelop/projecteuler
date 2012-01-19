import Data.Ratio
import System
import Control.Monad

func :: Int -> Rational
func lim = 1 + f lim
    where
        f 0 = 0
        f lim = 1 / (2 + f (lim - 1))

solve x = length . filter f $ map func [1..x]
    where
        f :: Rational -> Bool
        f a = let n = tr $ numerator a
                  d = tr $ denominator a
              in n > d
        tr :: Integer -> Int
        tr a = length $ show a

main = liftM ( read . head ) getArgs >>= print . solve
