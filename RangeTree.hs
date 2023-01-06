{-# LANGUAGE RecordWildCards #-}
module RangeTree where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group)

data OneTree =
    Onempty | Oleaf Int |
    Onode {
        n :: Int,
        less :: OneTree,
        more :: OneTree}

newtype Bunch = Bunch [Int] deriving (Eq,Show)

instance Ord Bunch where
    Bunch (x:_) <= Bunch (y:_) = x <= y

matchUp :: [Int] -> [Bunch]
matchUp = sort >>> group >>> map Bunch

makeOne [] = Onempty
makeOne [x] = Oleaf x
makeOne xs = Onode median (makeOne less) (makeOne more) where
    (less, median, more) = splitApart xs

rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n | low <=n && n<=high]
rangeOne low high Onode {..}
    | low <=n && n<=high =
        rangeLeft low high less ++ rangeRight low high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

rangeRight :: Int -> Int -> OneTree -> [Int]
rangeRight = _

rangeLeft :: Int -> Int -> OneTree -> [Int]
rangeLeft = _

