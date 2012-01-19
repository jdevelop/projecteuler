{-#LANGUAGE MultiParamTypeClasses#-}

import Data.Array
import Data.List (map,foldl,intercalate)

class Matrix m a where
    get :: Int -> Int -> m a -> a
    put :: Int -> Int -> a -> m a -> m a

data AMatrix a = AMatrix {
    w, h :: Int,
    content :: Array Int ( Array Int a )
}

instance Matrix AMatrix Int where
    get x y (AMatrix _ _ ctn) = (ctn ! x) ! y
    put x y val (AMatrix w' h' ctn) = let row = ctn ! x in AMatrix w' h' $ ctn // [(x, row // [(y,val)])]

instance (Show a) => Show (AMatrix a) where
    show (AMatrix w' h' con) = foldl ( showRow con ) "" [1 .. h']
        where
            showRow arr str i = str ++ "\n" ++ (intercalate "," $ map ( \j -> show $ (arr ! i) ! j ) [1 .. w'])

empty m n = AMatrix m n $ array (1,m) . zipWith (,) [1..m] . repeat . array (1,n) . zipWith (,) [1..n] $ repeat 0

sumLists :: [Integer] -> [Integer]
sumLists ps = 1:(sumLists' (tail ps) 1)
    where
        sumLists' (y:ys) x = let x' = y+x in x':(sumLists' ys x')
        sumLists' _ _ = []

buildMatrix :: Int -> [[Integer]]
buildMatrix w = iterate sumLists (replicate w 1)
