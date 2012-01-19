import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Function

buildPairs cnt = map f $ zip (C.lines cnt) [1..]
    where
        unp = fromIntegral . read . C.unpack
        f (l,c) = let (n:p:[]) = C.split ',' l
              in ((unp n, unp p),c)

cmp (a,b) (c,d) | a < 1 && c > 1 = LT
                | c < 1 && a > 1 = GT
                | a == c = b `compare` d
                | d > b = cmp (a / c,b) (c,d-b)
                | otherwise = cmp (a,b-d) (c/a,d)

main = C.readFile "base_exp.txt" >>= print . maximumBy (cmp `on` fst) . buildPairs
