import Data.List
import qualified Data.Map as DM

mapping = DM.fromList [
    ('I',1),
    ('V',5),
    ('X',10),
    ('L',50),
    ('C',100),
    ('D',500),
    ('M',1000)]

fromRomanian :: String -> Integer
fromRomanian [] = 0
fromRomanian ('I':'V':rest) = 4 + (fromRomanian rest)
fromRomanian ('I':'X':rest) = 9 + (fromRomanian rest)
fromRomanian ('X':'L':rest) = 40 +(fromRomanian rest)
fromRomanian ('X':'C':rest) = 90 +(fromRomanian rest)
fromRomanian ('C':'D':rest) = 400 +(fromRomanian rest)
fromRomanian ('C':'M':rest) = 900 +(fromRomanian rest)
fromRomanian (x:rest) = (DM.findWithDefault 0 x mapping) + (fromRomanian rest)

toRomanian x | x >= 1000 = 'M' : toRomanian (x-1000)
             | x >= 900 = 'C':'M' : toRomanian (x-900)
             | x >= 500 = 'D' : toRomanian (x-500)
             | x >= 400 = 'C':'D' : toRomanian (x-400)
             | x >= 100 = 'C' : toRomanian (x-100)
             | x >= 90 = 'X':'C' : toRomanian (x-90)
             | x >= 50 = 'L':toRomanian (x-50)
             | x >= 40 = 'X':'L':toRomanian (x-40)
             | x >= 10 = 'X':toRomanian (x-10)
             | x == 9 = "IX"
             | x >= 5 = 'V':toRomanian (x-5)
             | x == 4 = "IV"
             | x >=1 = 'I':toRomanian (x-1)
             | otherwise = []

solve :: [String] -> Int
solve = foldr f 0 
    where
        f x a = let l1 = length x
                    l = toRomanian . fromRomanian $ x
                    l2 = length l
                    diff = l1-l2
                in a + if diff >= 0 then diff else error (x ++ ":" ++ l)

main = readFile "roman.txt" >>= print . show . solve . lines
