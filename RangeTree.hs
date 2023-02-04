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
        resB = rangeTwo ll hh tree
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

rangeTwo _ _ Tnempty = []
rangeTwo low high tl@(Tleaf bun sid) =
    if isTSplitnode low high tl
    then rangeSide low high sid
    else []
rangeTwo low high o@(Tnode {..})
    | isTSplitnode low high o =
        secondRange low high False tless ++ secondRange low high True tmore
    | low <= tn = rangeTwo low high tless
    | otherwise = rangeTwo low high tmore

rangeSide low high = rangeOne (bother low) (bother high)

rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n|(low <= n) && (high >= n)]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeOne low high less ++ rangeOne low high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

secondRange :: Bunch -> Bunch -> Bool -> TwoTree -> [Bunch]
secondRange _ _ _ Tnempty = []
secondRange low high _ tleaf@(Tleaf _ _) = rangeTwo low high tleaf
secondRange low high lor Tnode {..}
    | high <= tn = secondRange low high lor tless
    | low > tn= secondRange low high lor tmore
    | lor = reportTwo low high tless ++ secondRange low high lor tmore
    | otherwise = reportTwo low high tmore ++ secondRange low high lor tless

dropDim Tnempty = Onempty
dropDim x = tside x

reportTwo low high = rangeSide low high . dropDim

isSplitnode low high Onode {n} = (low <= n) && (high >= n)
isTSplitnode low high node = (low <= tn node) && (high >= tn node)
