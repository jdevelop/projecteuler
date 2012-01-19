import Data.Array as A (array,bounds,Array,(!),elems)
import Data.List

type Matrix a = Array Int (Array Int a)

getBounds :: Matrix a -> (Int,Int,Int,Int)
getBounds src = (left,top,right,bottom)
    where
        (top,bottom) = bounds src
        (left,right) = bounds $ src ! top

buildMatrix :: (a -> b) -> Int -> Int -> [[a]] -> Matrix b
buildMatrix f w h content = array (1,h) . zip [1 .. h] $ map (array (1,w) . zip [1..w] . map f) content

verticals :: Matrix a -> [[a]]
verticals src = map accumColumn [left .. right]
    where
        (left,top,right,bottom) = getBounds src
        accumColumn col = foldr ( \i acc -> ((src ! i) ! col):acc ) [] [top..bottom]

horizontals :: Matrix a -> [[a]]
horizontals = map elems . elems

diagonals :: Matrix a -> [[a]]
diagonals src = (map (buildIdx (+1) (+1) ) mainStart) ++ 
                (map (buildIdx (flip (-) 1) ( + 1)) sideStart)
    where
        (left,top,right,bottom) = getBounds src
        h = bottom - top + 1
        w = right - left + 1
        mainStart = zip ( repeat left ) [ bottom, bottom - 1 .. top] ++ zip [left + 1 .. right] (repeat top)
        sideStart = zip [ left .. right ] ( repeat top ) ++ zip ( repeat right ) [ top + 1 .. bottom ]
        buildIdx opX opY (x,y) | (x >= left && x <=right && y >= top && y <= bottom ) = (src ! y) ! x : buildIdx opX opY (opX x, opY y)
                             | otherwise = []

getProducts :: Integral a => [a] -> [a]
getProducts (a:b:c:d:xs) = a*b*c*d : getProducts (b:c:d:xs)
getProducts _ = []

calculate :: (Integral a, Read b, Integral b, Ord b) => Int -> Int -> ( a -> b ) -> [[a]] -> b
calculate w h f src = maximum [vers,horzs,diags]
    where
        matrix = buildMatrix f w h src
        (vers:horzs:diags:_) = map ( (.) (maximum . getProducts) ) $ [verticals, horizontals, diagonals] :: [[b]]
