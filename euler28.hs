import Data.List

spiral d = 1 : (take x . map ((+1) . sum) . tail . inits . concatMap (replicate 4) $ [2,4..])
    where
        x = 4 * length [1,3 .. d-1]
