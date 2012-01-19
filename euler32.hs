import Data.List as DL
import Data.Set as DS

toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

lst = DL.sum . DS.toList . DS.fromList . DL.concatMap snd . DL.filter cmp . DL.map (\a@(_:_:z) -> (DL.concatMap toDigits a, z)) $ [[x,y,x*y] | x<-[3..9], y<-[1000..9999]] ++ [[x,y,x*y] | x<-[11..99], y<-[100..999]]
    where
        cmp (x,y) = let c = DS.fromList x
                    in DL.length x == 9 && DS.size c == 9 && not (DS.member 0 c)

main = print $ lst
