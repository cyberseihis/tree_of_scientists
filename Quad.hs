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
import Data.List (nub, sortOn, sort)
import Data.Maybe (mapMaybe)

data Qtree =
    Qempty | Qleaf Point |
    Qtree {m::Point,ne::Qtree,se::Qtree,sw::Qtree,nw::Qtree}
    deriving (Eq,Show)

bls = (,) <$> [False,True] <*> [False,True]
fielt = zip bls [ne,nw,se,sw]

compQuarts x y = (x <= y, other x <= other y)

makeQtree :: [Point] -> Qtree
makeQtree [] = Qempty
makeQtree [x] = Qleaf x
makeQtree points =
    let m = meanp points
        g n = makeQtree . filter ((==n).compQuarts m) $ points
        [ne,nw,se,sw] = g <$> bls
    in Qtree {m=m, ne=ne, se=se, sw=sw, nw=nw}

rangeQt :: Point -> Point -> Qtree -> [Point]
rangeQt _ _ Qempty = []
rangeQt low high (Qleaf x) = [x | isCovered low high x]
rangeQt low high qt@(Qtree {m}) =
    let [(j,i),(h,k)] = compQuarts m <$> [low,high]
        hj = mapMaybe (`lookup` fielt) . nub $ [(j,i),(h,i),(j,k),(h,k)]
    in concatMap (rangeQt low high) $ hj <*> [qt]
