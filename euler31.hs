coins 1 = 1
coins 2 = 2
coins 3 = 5
coins 4 = 10
coins 5 = 20
coins 6 = 50
coins 7 = 100
coins 8 = 200


calculate amt = calculate' amt 8
    where
        calculate' a c | a == 0 = 1
                       | a < 0 || c == 0 = 0
                       | otherwise = calculate' a (c - 1) + calculate' (a - coins c) c
