import Data.Numbers.Primes
import qualified Data.List as DL
import qualified Data.Map as DM

sopf :: Int -> Int
sopf = sum . DL.nub . primeFactors

type Cache = DM.Map Int Int

k :: Cache -> Int -> (Cache, Int)
k initCache n = let (c,v) = foldr f (initCache,0) [n - 1, n-2 .. 1]
                    res = (sopf(n) + v) `div` n
                in (DM.insert n res c, res)
  where
    f j (c,r) | DM.member (n-j) c = (c, r + (sopf j) * (c DM.! (n-j)))
              | otherwise = let (c', v) = k c (n-j)
                                result = (sopf j) * v
                            in (DM.insert j result c', r + result)
