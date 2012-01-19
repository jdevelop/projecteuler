primes=2:3:sieve (tail primes) [5,7..]

sieve (p:tp) list = left++sieve tp (filter nd right)
    where (left, right) = break (>=p*p) list
          nd a = (0 /= a`mod`p)

sumP n = calc primes
    where
        calc (x:xs) | x < n = x + calc xs
                    | otherwise = 0
