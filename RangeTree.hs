{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module RangeTree (TwoTree (..),makeRange,rangeRange) where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group, partition, singleton, groupBy, sortOn)

data OneTree =
    Onempty | Oleaf Bunch |
    Onode {
        n :: Bunch,
        less :: OneTree,
        more :: OneTree} deriving (Eq,Show)

data TwoTree =
    Tnempty | Tleaf { tn :: Bunch, tside :: OneTree} |
    Tnode {
        tn :: Bunch,
        tside :: OneTree,
        tless :: TwoTree,
        tmore :: TwoTree} deriving (Eq,Show)

newtype Bunch = Bunch {unBunch:: [Point]} deriving (Show)

instance Eq Bunch where
    Bunch (x:_) == Bunch (y:_) = x <= y && x >= y
    
instance Ord Bunch where
    Bunch (x:_) <= Bunch (y:_) = x <= y

makeRange :: [Point] -> TwoTree
makeRange = matchUp x >>> makeTwo

rangeRange :: Point -> Point -> TwoTree -> [Point]
rangeRange low high tree =
    let ll = Bunch [low]
        hh = Bunch [high]
        resB = rangeTwo ll hh Idk tree
    in concatMap unBunch resB

bother :: Bunch -> Bunch
bother (Bunch x) = Bunch . map other $ x

matchUp x = sortOn x >>> groupBy (\pa pb->x pa == x pb) >>> map Bunch

makeOne [] = Onempty
makeOne [x] = Oleaf x
makeOne xs =
    Onode median mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeOne (median:less)
    mkMore = makeOne more


makeTwo [] = Tnempty
makeTwo [x] = Tleaf x (makeSide [x])
makeTwo xs =
    Tnode median (makeSide xs) mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeTwo (median:less)
    mkMore = makeTwo more

makeSide :: [Bunch] -> OneTree
makeSide = makeOne . matchUp y . map other . concatMap unBunch

rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n|(low <= n) && (high >= n)]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeOne low high less ++ rangeOne low high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

data Farse = Idk | Lft | Rgt deriving (Eq)

rangeTwo :: Bunch -> Bunch -> Farse -> TwoTree -> [Bunch]
rangeTwo _ _ _ Tnempty = []
rangeTwo low@(Bunch(Pointy {}:_)) high _ t = rangeOne low high . tside $ t
rangeTwo low high _ Tleaf {..} =
    if (low <= tn) && (high >= tn)
    then rangeOne (bother low) (bother high) tside
    else []
rangeTwo low high lor Tnode {..}
    | high <= tn = rangeTwo low high lor tless
    | low > tn= rangeTwo low high lor tmore
    | lor == Idk = rangeTwo low high Lft tless ++ rangeTwo low high Rgt tmore
    | lor == Rgt = rangeTwo (bother low) (bother high) lor tless ++ rangeTwo low high lor tmore
    | otherwise = rangeTwo (bother low) (bother high) lor tmore ++ rangeTwo low high lor tless

isSplitnode low high Onode {n} = (low <= n) && (high >= n)
