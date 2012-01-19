import Data.Numbers.Primes
import qualified Data.Array.IO as A
import Data.List
import Data.Ratio
import Data.Function
import Control.Monad
import System

toDigits ::  Integral t => t -> [t]
toDigits x | x < 10 = [x]
           | otherwise = x `mod` 10 : toDigits (x `div` 10)

totientPrime x = numerator $ (x % 1) * (1%1 - 1 % x)

totient arr x ps = do
    let fstDivisor = head $ dropWhile ((/=0) . ( x `mod` )) ps
        sndDivisor = x `div` fstDivisor
    n <- A.readArray arr fstDivisor
    m <- A.readArray arr sndDivisor
    let d = gcd fstDivisor sndDivisor
    phiD <- A.readArray arr d
    return $ (n*m*d) `div` phiD

buildArray :: Integer -> [Integer] -> IO (A.IOArray Integer Integer)
buildArray lim ps = do
    arr <- A.newArray (1,lim) 0 :: IO (A.IOArray Integer Integer)
    mapM_ (fillPrimes arr) ps
    A.writeArray arr 1 1
    A.writeArray arr 2 1
    mapM_ (upd arr) [3..lim]
    return arr
    where
        fillPrimes arr p = A.writeArray arr p (totientPrime p)
        upd arr i = do
            l <- A.readArray arr i
            if l == 0 
            then do
                nTot <- do
                    if (i `mod` 2 == 0)
                        then do
                            let ni = i `div` 2
                            nv <- A.readArray arr ni
                            let nT = if ni `mod` 2 == 0 then nv * 2 else nv
                            A.writeArray arr i nT
                            return nT
                        else do 
                            nT <- totient arr i ps
                            A.writeArray arr i nT
                            return nT
                mapM_ (updRest arr i nTot) (takeWhile ((<=lim) . (*i)) [1..])
            else do return ()
        updRest arr i x t = do
            nT <- A.readArray arr t
            let d = gcd t i
            phiD <- A.readArray arr d
            A.writeArray arr (i*t) ((nT*x*d) `div` phiD)

main = do
    lim <- liftM ( read . head ) getArgs
    let cache = takeWhile ( <= lim ) primes
    (print . sum . tail ) =<< (A.getElems =<< buildArray lim cache)
