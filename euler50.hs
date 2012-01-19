import Data.Set as DS
import Data.List as DL
import System
import Control.Monad (liftM)

primes :: [Int]
primes = 2:3:[x | x <- [4 .. ], isPrime x]
isPrime x = all (/=0) . DL.map(x `mod`) $ takeWhile ((<=x) . (^2)) primes

primeCache l = DS.fromList $ takeWhile ( < l ) primes

takePrime _ _ _ _ [] = []
takePrime c lim s ln (x:xs) | s+x >= lim = []
                            | otherwise = let ln' = ln+1
                                              s' = s+x
                                          in (if DS.member s' c then [(s',ln')] else []) : takePrime c lim (s+x) (ln+1) xs

main = do
    lim <- liftM (read . head) getArgs
    let cache = primeCache lim
    print $ show . maximumBy (\(_,s) (_,s') -> s `compare` s') . concat . concatMap (takePrime cache lim 0 0) . init . tails $ takeWhile (<lim) primes
