import Data.Array.Diff
import Data.List
import Data.Maybe

type Chain = (Int, Int)
type Chains = Array Int [Chain]
type Cache = Array Int Int

type Generator = Int -> Chain

buildChains :: [Generator] -> Chains
buildChains = foldr acc (listArray (0,99) (repeat [])) . concatMap f
    where
        f g = map mkChain . dropWhile ((<1010) . fst) . takeWhile ((< 10000) . fst) . map g $ [1..]
        mkChain (n,i) = let h = n `div` 100
                            l = n `mod` 100
                        in z (h,(l,i))
        z (h,m) a = let l = a ! h
                    in case l of
                        [] -> a // [(h,[m])]
                        x -> a // [(h,m:x)]
        acc y a = y a

chains = buildChains [
        \x -> (x*(x+1) `div` 2, 3),
        \x -> (x^2,4),
        \x -> (x*(3*x-1) `div` 2, 5),
        \x -> (x*(2*x-1), 6),
        \x -> (x*(5*x-3) `div` 2, 7),
        \x -> (x*(3*x-2), 8)
         ]

findChain :: Int -> Int -> Cache -> Chains -> Int -> Maybe (Cache)
findChain b 0 c _ i = if i == b then Just c else Nothing
findChain b len c a i = let vals = a ! i
                      in case vals of
                        [] -> Nothing
                        xs -> listToMaybe . concatMap ( maybeToList . uncurry f ) $ filter ( (== (-1)) . (c !) . snd) xs
    where
        f idx s = findChain b (pred len) ( c // [(s,i*100+idx)] ) a idx

solve = fmap (sum . elems) .  head . nub . filter ( /= Nothing) $ map ( \x -> findChain x 6 (listArray (3,8) (repeat (-1))) chains x ) [0..99]
