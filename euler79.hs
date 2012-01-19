import Data.List as DL
import Data.Set as DS
import Data.ByteString.Char8 as B

buildChars :: [[Char]] -> [Char]
buildChars = DS.toList . DS.fromList . DL.concat

readKeys :: B.ByteString -> [[Char]]
readKeys = DL.map (B.unpack) . B.lines

solve s = let keys = readKeys s
              chars = buildChars keys
          in DL.filter ( \p -> DL.and $ DL.map (flip f p) keys) $ DL.permutations chars
    where
        f [] _ = True
        f _ [] = False
        f [x] [y] = x == y
        f (k:ks) p = let rest = DL.dropWhile ( /= k ) p
                 in if DL.null rest then False else f ks (DL.tail rest)
