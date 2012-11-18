import qualified Data.Map as DM
import System.Environment
import Control.Monad
import System.Environment

pentagonal x = x*(3*x-1) `div` 2

type Cache = DM.Map Int Integer

generalized_pentagonal n | n < 0          = 0
                         | n `mod` 2 == 0 = pentagonal (n `div` 2 + 1)
                         | otherwise      = pentagonal (-(n `div` 2 + 1))

solve n cache = g 0 (-1) 0 cache
  where
    g r f i c | k > n = (n, r, DM.insert n r c)
              | otherwise = g ( r + f' * (c DM.! (n - k)) ) f' (succ i) c
      where
        k = generalized_pentagonal(i)
        f' | i `mod` 2 == 0 = (-f)
           | otherwise = f

generate = f 1 $ DM.singleton 0 1
  where
    f x c = let (n, r, c') = solve x c
            in (n,r) : f (x+1) c'

main = do
  lim <- liftM (read . head) getArgs
  print . head $ dropWhile ( ( /=0 ) . (`mod` lim) . snd) generate
