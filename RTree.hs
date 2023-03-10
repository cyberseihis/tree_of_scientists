{-# LANGUAGE DeriveGeneric #-}
module RTree where
import Kdtree
import Control.Arrow (Arrow((&&&), second))
import Data.List (nub, sortOn, singleton)
import Data.List.Split (splitPlaces)
import GHC.Generics

data Mbr = Mbr {sx::Double,sy::Double,mx::Double,my::Double} deriving (Eq,Show,Generic)
data RTree = Rempty | Rleaf Point | Rnode Mbr [RTree] deriving (Eq,Show,Generic)

doubleMbr :: Point -> Point -> Mbr
doubleMbr (Pointx sx sy _) (Pointx mx my _) = Mbr sx sy mx my

makeRtree :: [Point] -> RTree
makeRtree [] = Rempty
makeRtree [x] = Rleaf x
makeRtree points =
    Rnode (bindRect points) . map makeRtree . cutUp $ points

rangeRt :: Point -> Point -> RTree -> [Point]
rangeRt _ _ Rempty = []
rangeRt low high (Rleaf point) = [point | isCovered low high point]
rangeRt low high (Rnode mbr children)
    | collides mbr $ doubleMbr low high = concatMap (rangeRt low high) children
    | otherwise = []

collides (Mbr x1 y1 x2 y2) (Mbr x3 y3 x4 y4) =
  x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3

bindRect :: [Point] -> Mbr
bindRect points = Mbr sx sy mx my where
    ((sx,mx),(sy,my)) = (f.map x&&&f.map y) points
    f = minimum &&& maximum

uniqz :: [Point] -> (Int,Int)
uniqz = nf.map x&&&nf.map y where nf = length.nub

badSort :: [Point] -> [Point]
badSort points = sortOn dim points
    where (nx,ny) = uniqz points
          dim = if nx>ny then x else y

cutUp xs =
    let x = length xs
        (d,m) = divMod x 10
        ar = zipWith (+) (replicate 10 d) $ replicate m 1 ++ repeat 0
    in takeWhile (not.null) $ splitPlaces ar xs
