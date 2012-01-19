import Data.List
import Data.ByteString.Char8 as BS (readFile, lines, unpack, split)

border = 10^6

sumLines [_] _ = []
sumLines (a:b:restU) (c:d:restC) | a == border && c == border = d : sumLines (b:restU) (d:restC)
                                 | b + d > c + d = (c+d) : sumLines (b:restU) (c+d:restC)
                                 | otherwise = (b+d) : sumLines (b:restU) (b+d:restC)


calculate a@(x:xs) = foldr (\x acc -> 0 : (sumLines acc x)) (replicate len border) . reverse . map (border:) $ a
    where
        len = length x + 1

main = BS.readFile "81" >>= print . calculate . map (map (read . unpack)) . map (BS.split ',') . BS.lines
