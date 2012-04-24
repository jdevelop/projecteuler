import qualified Data.Set as DS
import Data.List

buildNGon z@((a,b,c):_) xs | DS.size xs == 1 = g (DS.elems xs)
                           | otherwise = concatMap f nums
  where
    nums = filter (\(x,y) -> x+y == a+b) 
      $ [(x,y) | x<-DS.toList xs, y <- DS.toList (DS.delete x xs)]
    f (x,y) = buildNGon ((x,c,y):z) (DS.delete y $ (DS.delete x xs))
    g [x] | x+c+b1 == a+b+c = [(x,c,b1):z]
          | otherwise = []
    (a1,b1,c1) = last z

generateNGons xs = concatMap f seeds
  where
    seeds = [(x,y,z) | x <- lst, 
                       y <- DS.toList ( DS.delete x xs), 
                       z <- DS.toList ( DS.delete y $ DS.delete x xs)]
    lst = DS.toList xs
    f z@(a,b,c) = buildNGon [z] (DS.delete c . DS.delete b $ DS.delete a xs)

normalizeNGon xs = f xs
  where
    (minItem,_,_) = minimumBy (\(a,_,_) (b,_,_) -> a `compare` b) xs
    f z@((a,b,c):rest) | a == minItem = z
                       | otherwise = f (rest ++ [(a,b,c)])

solve = DS.findMax . DS.fromList . map (f . normalizeNGon . reverse) . generateNGons . DS.fromList
  where
    f = concatMap (\(a,b,c) -> show a ++ show b ++ show c)
