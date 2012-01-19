daysR = [31,28,31,30,31,30,31,31,30,31,30,31]
daysL = [31,29,31,30,31,30,31,31,30,31,30,31]

days from to = map days' [ from .. to ]
    where
        days' y | ( y `mod` 100 /= 0 && y `mod` 4 == 0) || ( y `mod` 100 == 0 && y `mod` 400 == 0) = daysL
                | otherwise = daysR


calculate to = flip (-) 2 . snd . foldl go (0,0) . concat $ days 1900 to
    where
        go (cur,num) x | newCur `mod` 7 == 6 = (newCur, num + 1)
                       | otherwise = (newCur,num)
            where 
                newCur = cur + x
