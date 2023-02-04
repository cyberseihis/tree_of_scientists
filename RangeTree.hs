{-# LANGUAGE RecordWildCards #-}
module RangeTree (TwoTree (..),makeRange,rangeRange) where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group, partition, singleton, groupBy, sortOn)
import Data.Maybe (mapMaybe)

data TwoTree =
    Tnempty | Tleaf { tn :: Point, tdata :: [Point], tside :: TwoTree} |
    Tnode {
        tn :: Point,
        tside :: TwoTree,
        tless :: TwoTree,
        tmore :: TwoTree} deriving (Eq,Show)

makeRange :: [Point] -> TwoTree
makeRange = makeTwo

rangeRange :: Point -> Point -> TwoTree -> [Point]
rangeRange low high = rangeTwo low high Idk

allSorted :: Ord a => [a] -> Bool
allSorted = and . (zipWith (<=) <*> tail)
isK xs = allSorted xs && allSorted (reverse xs)

makeTwo [] = Tnempty
makeTwo [x] = Tleaf x [x] (makeSide [x])
makeTwo xs@(p@(Pointy {}):_)
    | isK xs = Tleaf p xs Tnempty
makeTwo xs =
    Tnode median (makeSide xs) mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeTwo (median:less)
    mkMore = makeTwo more

makeSide :: [Point] -> TwoTree
makeSide = makeTwo . map other . filter isx
    where isx Pointx {} = True
          isx _ = False 

data Farse = Idk | Lft | Rgt | Dwn deriving (Eq)

rangeTwo :: Point -> Point -> Farse -> TwoTree -> [Point]
rangeTwo _ _ _ Tnempty = []
rangeTwo low@Pointx {} high Dwn t =
    rangeTwo (other low) (other high) Dwn . tside $ t
rangeTwo low high Dwn Tleaf {..} =concat[tdata|(low <= tn) && (high >= tn)]
rangeTwo low high _ Tleaf {..} =
    if (low <= tn) && (high >= tn)
    then rangeTwo (other low) (other high) Dwn tside
    else []
rangeTwo low high lor Tnode {..}
    | high <= tn = rangeTwo low high lor tless
    | low > tn   = rangeTwo low high lor tmore
    | lor == Dwn = rangeTwo low high lor tless ++ rangeTwo low high lor tmore
    | lor == Idk = rangeTwo low high Lft tless ++ rangeTwo low high Rgt tmore
    | lor == Rgt = rangeTwo low high Dwn tless ++ rangeTwo low high lor tmore
    | otherwise  = rangeTwo low high Dwn tmore ++ rangeTwo low high lor tless
