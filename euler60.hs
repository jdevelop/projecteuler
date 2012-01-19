import Data.List
import Data.Function
import Data.Maybe
import Control.Monad
import System
import qualified Data.Set as DS
import qualified Data.Map as DM

primes = 2:3:[x|x<-[4..],isPrime x]
isPrime x = all (/=0) . map (x `mod`) . takeWhile((<=x) . (^2)) $ primes

ps = takeWhile (<100000000) primes
cache = DS.fromList ps

getExponent x | x < 10 = 1
              | otherwise = 1 + getExponent (x `div` 10)


splitPrimes x = concatMap f [1 .. (len `div` 2)]
    where
        len = getExponent x
        f e = let fe = len-e
                  f1 = x `div` (10^e)
                  t1 = x `mod` (10^e)
                  f2 = x `div` (10^fe)
                  t2 = x `mod` (10^fe)
              in map (\(a,b,c) -> (if a < b then (a,b) else (b,a))) $ filter (\(a,b,c) -> (
                    getExponent a + getExponent b == len) && DS.member a cache && DS.member b cache && DS.member c cache) 
                    [(f1,t1,t1*10^fe+f1),(f2,t2,t2*10^e+f2)]
        init' [] = []
        init' xs = init xs

buildMap = foldr f DM.empty $ concatMap splitPrimes ps
    where
        f (a,b) m = DM.alter (f' a) b $ DM.alter (f' b) a m
        f' v Nothing = Just $ DS.singleton v
        f' v (Just s) = Just $ DS.insert v s

traverseMap :: Int -> DM.Map Int (DS.Set Int) -> DS.Set Int -> Int -> [DS.Set Int]
traverseMap 0 _ xs _ = [xs]
traverseMap len m xs x | and (DS.fold ((:) . (flip DS.member) s) [] xs) = let vals = DS.toList s
                                                                              xs' = DS.insert x xs
                                                             in concatMap (traverseMap (pred len) m xs' ) $ filter ( not . (flip DS.member xs') ) vals
                       | otherwise = []
    where
        s = fromJust $ DM.lookup x m

solve len = sum . DS.toList . head $ concatMap (traverseMap len d DS.empty) ks
    where 
        d = buildMap
        ks = DM.keys d

verifyPrimeness [] = []
verifyPrimeness (x:xs) = concatMap makePrime (map ((,) x) xs) : verifyPrimeness xs
    where
        makePrime (a,b) = let f1 = getExponent a
                              f2 = getExponent b
                          in [a*10^f2+b,b*10^f1+a]

main = do
    liftM (read . head) getArgs >>= print . solve
