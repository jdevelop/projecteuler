import Data.List as DL
import Data.Set as DS

primes = 2 : 3 : 5 : [ x | x <- [6 .. ], isPrime x ]

isPrime x = all (/=0) . DL.map ( x `mod` ) $ takeWhile ((<=x) . (^2)) primes

buildFraction r x d | x < d = buildFraction r (x*10) d
                    | x `mod` d == 0 = [x `div` d]
                    | otherwise = chk (x `div` d) (x `mod` d)
    where
        chk a b | DS.member b r = if DS.member a r then [] else [a]
                | otherwise = a : buildFraction (DS.insert b r) b d

solve l = fst . maximumBy cmp . DL.map bld $ takeWhile (<= l) primes
    where 
        cmp (_,a) (_,b) = a `compare` b
        bld x = (x,length $ buildFraction DS.empty 10 x)
