import Data.List
import Data.Numbers.Primes
import qualified Data.Set as DS

buildCache lim ps = map DS.toList $ foldr f [DS.empty,DS.empty,DS.empty] ps
    where
        f n ms = zipWith (z n) ms [2..4]
        z n m p = let x = n^p
                  in if x < lim then DS.insert x m else m

solve lim = let sqrtD = round ( sqrt ( fromIntegral lim ) )
                (a:b:c:[]) = buildCache lim $ takeWhile ( < sqrtD ) primes
            in DS.size $ DS.fromList [x+y+z | x <- a, y <- b, z <- c,  x+y+z < lim]
