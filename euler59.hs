import Data.ByteString.Char8 as C
import Data.List as DL
import Data.Bits
import Data.Char

content :: C.ByteString -> [Int]
content = DL.map (read . C.unpack) . C.split ','

sequences = [a:b:c:[] | a<-['a'..'z'], b<-['a'..'z'],c<-['a'..'z']]

commons ::  [ByteString]
commons = DL.map pack ["the","have","with"]

tryPass ::  ByteString -> ByteString -> [ByteString]
tryPass xs ks = let s = C.pack $ C.zipWith bXor xs ks'
                in if DL.and (DL.map ( `C.isInfixOf` s) commons) then [s] else []
    where
        bXor a b = chr(ord a `xor` ord b)
        ks' = C.concat . DL.concat $ DL.replicate (C.length xs `div` 3 + 1) [ks]

sumAscii :: ByteString -> Int
sumAscii = C.foldr (\c acc -> ord c + acc) 0

main = do
    ctn <- (C.pack . DL.map chr . content) `fmap` C.readFile "cipher1.txt"
    print . DL.map sumAscii . DL.concat . DL.filter (not . DL.null) $ DL.map ( tryPass ctn . C.pack ) sequences
