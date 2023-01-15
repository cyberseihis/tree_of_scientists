{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module RangeTree where
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
    Tnempty | Tleaf { tval :: Bunch, tlide :: OneTree} |
    Tnode {
        tn :: Bunch,
        tside :: OneTree,
        tless :: TwoTree,
        tmore :: TwoTree} deriving (Eq,Show)

newtype Bunch = Bunch {unBunch:: [Point]} deriving (Eq,Show)

instance Ord Bunch where
    Bunch (x:_) <= Bunch (y:_) = x <= y

matchUp = sort >>> groupBy (\pa pb->x pa == x pb) >>> map Bunch
matchUpy = sortOn y >>> groupBy (\pa pb->y pa == y pb) >>> map Bunch

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
makeSide = makeOne . matchUpy . map other . concatMap unBunch

rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n|(low <= n) && (high >= n)]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeLeft low high less ++ rangeRight low high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

rangeLeft _ _ Onempty = []
rangeLeft low high (Oleaf n) = [n | low <= n]
rangeLeft low high Onode {..}
    | low > n= rangeLeft low high more
    | otherwise = report more ++ rangeLeft low high less

rangeRight _ _ Onempty = []
rangeRight low high (Oleaf n) = [n |high >= n]
rangeRight low high Onode {..}
    | high <= n  = rangeRight low high less
    | otherwise = report less ++ rangeRight low high more

report Onempty = []
report (Oleaf n) = [n]
report Onode {..} = concatMap report [less,more]

isSplitnode low high Onode {n} = (low <= n) && (high >= n)
