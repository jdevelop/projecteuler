import System
import Control.Monad (liftM)
import Control.Concurrent
import Data.List.Split

fromDigits x | x < 10 = [x]
             | otherwise = x `mod` 10 : fromDigits (x `div` 10)

isReversible :: [Int] -> Bool
isReversible xs = f 0 $ zip xs (reverse xs)
  where
    f _ [] = True
    f pred ((a,b):r) = let z = pred + a + b
                           (x,y) = divMod z 10
                       in if (y `mod` 2 == 1)
                          then f x r
                          else False

solve l h = length . filter (isReversible . fromDigits) $ filter ((/=0) . (`mod` 10)) [l .. h]

main = do 
  (cores:upper:_) <- liftM (map read ) getArgs  :: IO [Int]
  let (x,y) = upper `divMod` cores
  latch <- newMVar cores
  result <- newMVar 0
  mapM_ (forkIO . calculate latch result) $ getIntervals upper cores
  countdownLatch 100 latch
  readMVar result >>= print
  where
    getIntervals u c = let (intervalSize, lastIntervalSize) = u `divMod` c
                           ivs = takeWhile ((<= u) . snd) $ map (\i -> ((pred i)*intervalSize,i*intervalSize-1)) [1..]
                       in  if (lastIntervalSize == 0) 
                           then ivs
                           else (intervalSize * c,u):ivs
    calculate latch result (l,h) = do
      let z = solve l h
      current <- readMVar result
      modifyMVar_ result (return . ( + z))
      modifyMVar_ latch ( return . pred )
    countdownLatch sleepTime latchMVar = do
      currentThreads <- takeMVar latchMVar
      if currentThreads <= 0
        then return ()
        else do 
          putMVar latchMVar currentThreads
          threadDelay sleepTime
          countdownLatch sleepTime latchMVar
