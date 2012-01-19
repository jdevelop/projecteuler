import Data.List

triples p = filter (\(a,b,c) -> a > 0 && b > 0 && c > 0 && a < b && b < c && a^2 + b^2 == c^2) $ map buildTriple [1..p-1]
    where 
        buildTriple a = let b = (sqrP-2*p*a) `div` (2 * (p - a))
            in (a,b,p-a-b)
        sqrP = p^2

findMaxTriple p = fst $ foldr comp (0,0) [1..p]
    where
        comp x (s,l) = let len = length $ triples x
                     in if len > l then (x,len) else (s,l)
