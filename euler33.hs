import Data.List

fractions l = filter chk $ [ ((x,y),cancel x y) | x<-[11..l],y<-[x+1..l], x `mod` 10 /= 0 || y `mod` 10 /= 0 ]
    where
        chk ((a,b),[(c,d)]) = let d1 = gcd a b
                                  d2 = gcd c d
                              in d1 /= 1 && (a `div` d1, b `div` d1) == (c `div` d2, d `div` d2)
        chk (_,[]) = False

cancel x y | a == c = [(b,d)]
           | a == d = [(b,c)]
           | b == c = [(a,d)]
           | b == d = [(a,c)]
           | otherwise = []
    where
        a = x `div` 10
        b = x `mod` 10
        c = y `div` 10
        d = y `mod` 10

solve = g . foldr f (1,1) $ map (head . snd ) $ fractions 99
    where
        f (a,b) (c,d) = (c*a,d*b)
        g (a,b) = let d = gcd a b
                  in b `div` d
