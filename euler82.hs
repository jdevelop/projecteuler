import Data.List
import qualified Data.PSQueue as Q
import qualified Data.Map as DM
import qualified Data.ByteString.Char8 as C8

parseContent :: C8.ByteString -> [[((Int,Int),Int)]]
parseContent s = map f $ zip (map (map readStr . C8.split ' ') . C8.lines $ s) [1..]
    where
        readStr = read . C8.unpack
        f (cols,row) = zipWith (\x y -> ((row, y),x)) cols [1..]

dijkstra vertex ws = g s ws results
    where
        (mW,mH) = fst $ DM.findMax ws
        s = DM.foldrWithKey f (Q.singleton vertex ( ws DM.! vertex)) ws
        results = DM.map ( const (maxBound::Int) ) ws
        f k v q | k == vertex = q
                | otherwise = Q.insert k maxBound q
        g q ws res = let z = Q.minView q
                     in 
                        case z of
                            Nothing -> res
                            Just (mb,q') -> if (Q.prio mb) == maxBound 
                                           then res
                                           else let (_,q'',res') = foldr g' (Q.prio mb,q',res) (getNearest (Q.key mb))
                                                in g q'' ws res'
        g' (x,y) (v,q,res) = let (res',q') = updateForPrio v q res (x,y)
                             in (v,q',res')
        getNearest (x,y) = filter (\(x,y) -> x <= mW && y > 0 && y <= mH) $ map (\(dx,dy) -> (x+dx,y+dy)) [(0,(-1)),(1,1),(0,1)]
        updateForPrio x q res k = let w = res DM.! k
                                      cw = ws DM.! k
                                      nw = cw + x
                                  in if nw < w
                                     then (DM.insert k nw res, Q.adjust (const nw) k q)
                                     else (res,q)
