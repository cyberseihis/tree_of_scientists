{-# LANGUAGE RecordWildCards #-}
module RTree where
import Kdtree
import Control.Arrow (Arrow((&&&), second))
import Data.List (nub, sortOn, singleton)

data Mbr = Mbr
    { sx :: Int
    , sy :: Int
    , mx :: Int
    , my :: Int } deriving (Eq,Show)

data RTree = Rempty | Rleaf Point | Rnode Mbr [RTree] deriving (Eq,Show)

singleMbr :: Point -> Mbr
singleMbr (Pointx x y) = Mbr x y x y

doubleMbr :: Point -> Point -> Mbr
doubleMbr (Pointx sx sy) (Pointx mx my) = Mbr sx sy mx my

makeRtree :: Int -> [Point] -> RTree
makeRtree _ [] = Rempty
makeRtree _ [x] = Rleaf x
makeRtree fanout points =
    Rnode mybr . map (makeRtree fanout) . cutUp fanout $ points
    where mybr = bindRect points
    
rangeRt :: Point -> Point -> RTree -> [Point]
rangeRt _ _ Rempty = []
rangeRt low high (Rleaf point) = [point | isCovered low high point]
rangeRt low high (Rnode mbr children)
    | collides mbr $ doubleMbr low high = concatMap (rangeRt low high) children
    | otherwise = []

collides (Mbr x1 y1 x2 y2) (Mbr x3 y3 x4 y4) =
  x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3

bindRect :: [Point] -> Mbr
bindRect points =
    let ((sx,mx),(sy,my)) = (f.map x&&&f.map y) points
        f = minimum &&& maximum
    in Mbr sx sy mx my

uniqz :: [Point] -> (Int,Int)
uniqz =
    nf.map x&&&nf.map y
    where nf = length.nub

badSort :: [Point] -> [Point]
badSort points = sortOn dim points
    where
    (nx,ny) = uniqz points
    dim = if nx>ny then x else y

cutUp :: Int -> [a] -> [[a]]
cutUp _ [] = [[]]
cutUp _ [x] = [[x]]
cutUp 1 xs = [xs]
cutUp n xs =
    let (d,m) = divMod (length xs) n
        nd = d + min 1 m
        (bef,aft) = splitAt nd xs
    in bef : cutUp (n-1) aft
