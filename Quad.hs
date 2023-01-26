{-# LANGUAGE NamedFieldPuns #-}
module Quad where
import Kdtree
import Data.List.NonEmpty (groupAllWith, toList)
import Data.Map.Strict ( Map, (!) )
import Control.Arrow (Arrow((&&&), first, second), (>>>))
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Data.List (nub)

data Qdree = Qiempty | Qdree Point (Map Quad Qtree) deriving (Eq,Show)

data Qtree = 
    Qempty | 
    Qtree 
    { m :: Point
    , ne :: Qtree
    , se :: Qtree
    , sw :: Qtree
    , nw :: Qtree
    } deriving (Eq,Show)

type Quad = (Bool,Bool)

compQuarts x y = (x <= y, other x <= other y)

emptyQuart = Map.fromList . zip [(x,y)|x<-bls,y<-bls] $ repeat []
    where bls = [False,True]

splitAround x =
    map (compQuarts x &&& id) >>>
    groupAllWith fst >>>
    map (toList >>> unzip >>> first head) >>>
    Map.fromList >>>
    (`Map.union` emptyQuart)

spreadAround x =
    splitAround x >>>
    Map.elems >>>
    map length >>>
    (maximum &&& minimum) >>>
    uncurry (-)

makeQtree :: [Point] -> Qtree
makeQtree [] = Qempty
makeQtree points =
    let (m,[ne,nw,se,sw]) = mkQ points
    in Qtree {m=m, ne=ne, se=se, sw=sw, nw=nw}

mkQ =
    removeEach >>>
    maximumBy (comparing $ uncurry spreadAround) >>>
    (fst &&& uncurry splitAround) >>>
    second (Map.map makeQtree >>> Map.elems)

fielt :: Quad -> Qtree -> Qtree
fielt (False,False) = ne
fielt (False,True) = nw
fielt (True,False) = se
fielt (True,True) = sw

rangeQt :: Point -> Point -> Qtree -> [Point]
rangeQt _ _ Qempty = []
rangeQt smol big qt@(Qtree {m}) =
    let (j,i) = compQuarts m smol
        (h,k) = compQuarts m big
        hj = map fielt . nub $ [(j,i),(h,i),(j,k),(h,k)]
        ww = concatMap (rangeQt smol big) $ hj <*> [qt]
        isco = isCovered smol big m 
        hm = if isco then m:ww else ww
    in hm
    

removeEach xs = zip xs (map (`List.delete` xs) xs)
