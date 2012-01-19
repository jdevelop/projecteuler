import Data.Map as DM
import Data.List as DL

factorial 0 = 1
factorial x = product [1..x]

facts l = foldr (\x c -> DM.insert x (factorial x) c) DM.empty [0..l]

count z l = let c = facts l 
          in DL.length $ DL.filter (chk c) [ (n,r) | n <- [1..l], r <- [1..n] ]
    where
        chk c (a,b) = (c ! a) `div` ((c ! b)*(c ! (a - b))) > z
