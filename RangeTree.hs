{-# LANGUAGE RecordWildCards #-}
module RangeTree (TwoTree (..),makeRange,rangeRange) where
import Kdtree

data TwoTree =
    Tnempty | Tleaf { tn :: Point, tdata :: [Point], tside :: TwoTree} |
    Tnode { tn :: Point, tside :: TwoTree, tless :: TwoTree,
        tmore :: TwoTree} deriving (Eq,Show)

rangeRange :: Point -> Point -> TwoTree -> [Point]
rangeRange low high = rangeTwo low high Idk

allSorted :: Ord a => [a] -> Bool
allSorted = and . (zipWith (<=) <*> tail)

makeRange :: [Point] -> TwoTree
makeRange [] = Tnempty
makeRange [x] = Tleaf x [x] (makeSide [x])
makeRange xs@(p@(Pointy {}):_)
    | allSorted xs && allSorted (reverse xs) = Tleaf p xs Tnempty
makeRange xs =
    Tnode median (makeSide xs) mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeRange (median:less)
    mkMore = makeRange more

makeSide :: [Point] -> TwoTree
makeSide = makeRange . map other . filter isx
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
