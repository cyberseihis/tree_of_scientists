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

makeRtree :: Int -> [Point] -> RTree
makeRtree _ [] = Rempty
makeRtree _ [x] = Rleaf x
makeRtree fanout points =
    Rnode mybr . map (makeRtree fanout) . cutUp fanout $ points
    where mybr = bindRect points
    

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
