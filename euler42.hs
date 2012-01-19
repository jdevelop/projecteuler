import Data.List as DL
import Data.ByteString.Char8 as C
import Data.Set as DS
import Data.Map as DM
import Data.Maybe

triangles = DS.fromList . DL.takeWhile ( < (26^2) ) $ DL.map (\x -> x*(x+1) `div` 2) [1 .. ]

letters = DM.fromList $ DL.zip ['A','B' .. 'Z'] [1..]

getWords = DL.map ( DL.filter ( /= '"') . C.unpack )  . C.split ','

compute = DL.length . DL.filter (flip DS.member triangles ) . DL.map ( sum . DL.map (fromJust . flip DM.lookup letters) ) . getWords

main = C.readFile "words.txt" >>= print . compute
