import Data.List
import Data.ByteString.Char8 as BS (readFile, empty, readFile, split, unpack, lines)


sumLists [_] _ = []
sumLists (p:c:restU) (x:restC) | p+x > c+x = (p+x):sumLists (c:restU) restC
                               | otherwise = (c+x):sumLists (c:restU) restC

findMax a@(cur:lst) = foldl' (sumLists) init a
    where
        init = replicate (length cur + 1) 0

main = BS.readFile "2" >>= print . findMax . map ( map ( read . unpack ) ) . map (split (' ')). reverse . BS.lines
