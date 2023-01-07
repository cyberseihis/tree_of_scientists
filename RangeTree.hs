{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
module RangeTree where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group, partition, singleton)

data OneTree a =
    Onempty | Oleaf (Bunch a) |
    Onode {
        n :: Bunch a,
        less :: OneTree a,
        more :: OneTree a} deriving (Eq,Show)

newtype Bunch a = Bunch [a] deriving (Eq,Show)

instance Ord a => Ord (Bunch a) where
    (<=) :: Ord a => Bunch a -> Bunch a -> Bool
    Bunch (x:_) <= Bunch (y:_) = x <= y

matchUp :: Ord a => [a] -> [Bunch a]
matchUp = sort >>> group >>> map Bunch

makeRang :: Ord a => [a] -> OneTree a
makeRang = flip makeOne [] . matchUp

rangeRang low high ontree = concatMap (\(Bunch a)->a) $
    rangeOne ll hh ontree where
        ll = Bunch [low]
        hh = Bunch [high]

makeOne [] [] = Onempty
makeOne [x] [] = Oleaf x
makeOne [] [x] = Oleaf x
makeOne xs ys = Onode median (makeOne less (median:ls)) (makeOne more mr) where
    (less, median, more) = splitApart xs
    (ls,mr) = partition (<=median) ys

rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n | low <=n && n<=high]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeLeft low less ++ rangeRight high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

rangeLeft _ Onempty = []
rangeLeft low (Oleaf n) = [n | low <= n]
rangeLeft low Onode {..}
    | low > n = rangeLeft low more
    | otherwise = report more ++ rangeLeft low less

rangeRight _ Onempty = []
rangeRight high (Oleaf n) = [n | high > n]
rangeRight high Onode {..}
    | high <= n = rangeRight high less
    | otherwise = report less ++ rangeRight high more

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
