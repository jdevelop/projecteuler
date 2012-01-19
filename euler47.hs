import Data.List as DL
import Data.Set as DS
import System
import Control.Monad (liftM)

primes = 2:3:[x | x <-[4..], isPrime x]
isPrime x = all (/=0) . DL.map( x `mod` ) . takeWhile( (<=x) . (^2)) $ primes

primeFactors x (p:ps) | x == 1 = []
                      | x `mod` p == 0 = p : primeFactors (x `div` p) (p:ps)
                      | otherwise = primeFactors x ps


solve n ns | f (take n ns) = head ns
           | otherwise = solve n $ tail ns
    where
        f xs = DL.all (==n) $ DL.map (DS.size . DS.fromList . (flip primeFactors primes)) xs

main = liftM (read . head ) getArgs >>= print . (flip solve) [1..]
