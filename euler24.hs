mkInversion :: [Int] -> [a] -> [a]
mkInversion tbl symbols = foldl insert [] . reverse $ zip tbl symbols
    where
        insert lst (pos,item) | pos == 0 = item:lst
                              | otherwise = let (left,right) = splitAt pos lst
                                            in left ++ [item] ++ right

factorial 0 = 1
factorial n = product [1..n]

calculate :: Int -> [a] -> [a]
calculate n symbols = takeSymbol symbols cmd
    where
        cmd = permutation n (length symbols)
        takeSymbol _ [] = []
        takeSymbol (x:tbl) (0:cmds) = x : takeSymbol tbl cmds
        takeSymbol tbl (c:cmds) = let (left,right) = splitAt c tbl
                                  in head right : takeSymbol ( left ++ tail right) cmds
        

permutation n sz = go (n,1)
    where
       go (num, i) | i<=sz = let f = factorial (sz - i) 
                                 cn = (num `div` f )
                                 nn = (num `mod` f)
                              in cn:go (nn, succ i)
                   | otherwise = []
