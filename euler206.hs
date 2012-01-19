toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

match (_,(0:_:9:_:8:_:7:_:6:_:5:_:4:_:3:_:2:_:1:[])) = True
match _ = False

solve = fst . head . filter match . map ( \x -> (x,toDigits (x^2))) $ [x | x<-[1010101010 .. 1389027000], x `mod` 30 == 0]

main = print $ solve
