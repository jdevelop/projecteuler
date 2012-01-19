import Data.List
import qualified Data.Array as A
import qualified Data.ByteString.Char8 as C8

solve x@(a:as) = maximum $ map f perms
    where
        l = length a
        arr = A.listArray (1,length x) $ map (A.listArray (1,l)) x
        perms = map (zip [1..l]) $ permutations [1..l]
        f p = foldr (\(i,j) acc -> acc + (arr A.! i) A.! j) 0 p

parse :: C8.ByteString -> [[Int]]
parse = map (map (read . C8.unpack) . C8.split ' ') . C8.lines

main = C8.readFile "euler345.txt" >>= print . solve . parse
