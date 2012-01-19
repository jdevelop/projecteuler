import System
import Control.Monad


toDigits ::  Integral t => t -> [t]
toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits ( x `div` 10 )

isBouncing ::  Integer -> Bool
isBouncing = f EQ . toDigits
    where
        f _ [_] = False
        f EQ (x:y:ys) = f (x `compare` y) (y:ys)
        f GT (x:y:ys) | x >=y = f GT (y:ys)
                      | otherwise = True
        f LT (x:y:ys) | x <= y = f LT (y:ys)
                      | otherwise = True

solve ::  Integer -> Integer -> Integer
solve n d = f 0 10
    where
        f s x = let s' = if isBouncing x then succ s else s
                in if x*n == s'*d then x else f s' (x+1)

main = do
    (n:d:[]) <- getArgs
    print $ solve (read n) (read d)
