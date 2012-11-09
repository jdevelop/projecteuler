import qualified Data.Map as DM

type Args = (Integer, Integer)
type Cache = DM.Map Args Integer

type F = Integer -> Integer -> Cache -> (Integer, Cache)

p :: F
p 0 0 m = (1,m)
p _ 0 m = (0,m)
p n k m | k > n      = calc n n m
        | otherwise  = let (x, m')  = calc n (k-1) m 
                           (y, m'') = calc (n - k) k m'
                       in (x + y, m'')
  where
    calc :: F
    calc x y m = case DM.lookup (x,y) m of
                    Just z  -> (z,m)
                    Nothing -> let (z,m') = p x y m 
                               in (z, DM.insert (x,y) z m')

solve x m = let (x',m') = p x x m
            in (x,x') : solve (x+1) m'


main = print . head . dropWhile ((/=0) . (`mod` 100000) . snd ) $ solve 1 DM.empty