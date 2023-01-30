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

data Qtree =
    Qempty | Qleaf Point |
    Qtree {m::Point,ne::Qtree,se::Qtree,sw::Qtree,nw::Qtree}
    deriving (Eq,Show)

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
makeQtree [x] = Qleaf x
makeQtree points =
    (removeEach >>>
    maximumBy (comparing $ uncurry spreadAround) >>>
    (fst &&& uncurry splitAround) >>>
    second (Map.map makeQtree >>> Map.elems) >>>
    \(m,[ne,nw,se,sw])->Qtree {m=m, ne=ne, se=se, sw=sw, nw=nw}
    ) points

fielt = Map.fromList
    [((False,False),ne),((False,True),nw),((True,False),se),((True,True),sw)]

rangeQt :: Point -> Point -> Qtree -> [Point]
rangeQt _ _ Qempty = []
rangeQt low high (Qleaf x) = [x | isCovered low high x]
rangeQt low high qt@(Qtree {m}) =
    let [(j,i),(h,k)] = compQuarts m <$> [low,high]
        hj = map (fielt!) . nub $ [(j,i),(h,i),(j,k),(h,k)]
        ww = concatMap (rangeQt low high) $ hj <*> [qt]
    in if isCovered low high m then m:ww else ww

removeEach xs = zip xs (map (`List.delete` xs) xs)
