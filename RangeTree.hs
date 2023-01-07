{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module RangeTree where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group)

data OneTree =
    Onempty | Oleaf Int |
    Onode {
        n :: Int,
        less :: OneTree,
        more :: OneTree} deriving (Eq,Show)

newtype Bunch = Bunch [Int] deriving (Eq,Show)

instance Ord Bunch where
    Bunch (x:_) <= Bunch (y:_) = x <= y

matchUp :: [Int] -> [Bunch]
matchUp = sort >>> group >>> map Bunch

-- Replace Ints with Bunch
-- Oops I forgot to keep the split value for the leaves too
makeOne [] = Onempty
makeOne [x] = Oleaf x
makeOne xs = Onode median (makeOne less) (makeOne more) where
    (less, median, more) = splitApart xs

rangeOne :: Int -> Int -> OneTree -> [Int]
rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n | low <=n && n<=high]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeLeft low less ++ rangeRight high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

rangeLeft :: Int -> OneTree -> [Int]
rangeLeft _ Onempty = []
rangeLeft low (Oleaf n) = [n | low <= n]
rangeLeft low Onode {..}
    | low > n = rangeLeft low more
    | otherwise = report more ++ rangeLeft low less

rangeRight :: Int -> OneTree -> [Int]
rangeRight _ Onempty = []
rangeRight high (Oleaf n) = [n | high > n]
rangeRight high Onode {..}
    | high <= n = rangeRight high more
    | otherwise = report more ++ rangeRight high less

report Onempty = []
report (Oleaf n) = [n]
report Onode {..} = concatMap report [less,more]

isSplitnode low high Onode {n} = low <= n && n <= high

isSplitnodeBad low high o@(Onode {..}) = all (inrange low high) [o,less,more]
    where
    inrange _ _ Onempty = False
    inrange low high x = let v = grabVal x in low <= v && v <= high
    grabVal (Oleaf x) = x
    grabVal Onode {n} = n
