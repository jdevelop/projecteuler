import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List

size = 10

next n | even n = n `div` 2
       | otherwise = 3 * n + 1

chainlength arr n = do
  len <- if n < size then readArray arr n else return 0
  if len == 0 
    then do
      len <- chainlength arr (next n)
      when (n < size) $ writeArray arr n (len + 1)
      return (len + 1)
    else return len

collatz = runSTArray $ do
  cache <- newArray (1,size) 0 :: ST s (STArray s Integer Integer)
  writeArray cache 1 1
  forM_ [1..size] $ \n -> do
    len <- chainlength cache n
    return (n, len)
  return cache

main = print . fst . maximumBy (\x y -> snd x `compare` snd y) . assocs $ collatz
