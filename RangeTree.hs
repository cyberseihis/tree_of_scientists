{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
module RangeTree where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group, partition, singleton)

data OneTree a =
    Onempty | Oleaf a |
    Onode {
        n :: a,
        less :: OneTree a,
        more :: OneTree a} deriving (Eq,Show)

getOut (Oleaf x) = x
getOut Onode {n=x} = x

instance Ord a => Ord (OneTree a) where
    compare Onempty Onempty = EQ
    compare Onempty _ = LT
    compare _ Onempty = GT
    compare x y = compare (getOut x) (getOut y)

newtype Bunch a = Bunch [a] deriving (Eq,Show,Ord,Functor)

unBunch (Bunch a) = a

matchUp :: Ord a => [a] -> [Bunch a]
matchUp = sort >>> group >>> map Bunch

makeRang :: Ord a => [a] -> OneTree (Bunch a)
makeRang = flip makeOne [] . matchUp

rangeRang low high ontree = concatMap (\(Bunch a)->a) $
    rangeOne ll hh ontree where
        ll = Bunch [low]
        hh = Bunch [high]

makeOne [] [] = Onempty
makeOne [x] [] = Oleaf x
makeOne [] [x] = Oleaf x
makeOne xs ys =
    Onode median mkLess mkMore where
    (less, median, more) = splitApart xs
    (ls,mr) = partition (<=median) ys
    mkLess = makeOne less (median:ls)
    mkMore = makeOne more mr

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

tstt = rangeRang 40 70 xx where
    b = [3,8..100]++[63,68,43,68]
    xx = makeRang b

sprout :: OneTree (Bunch Point) -> OneTree (OneTree (Bunch Point))
sprout Onempty = Onempty
sprout x@(Oleaf a) = Oleaf x
sprout x@Onode {..} = Onode {n=sideTree n.report$x, less=sprout less, more=sprout more}

-- Previous point of comparison given to change root node to compare well
-- in both dimentions
sideTree :: Bunch Point -> [Bunch Point] -> OneTree (Bunch Point)
sideTree nn points =
    let x@Onode {..} = makeRang . map other . concatMap unBunch $ points
        y = x {n=surgery n nn}
    in y

surgery :: Bunch Point -> Bunch Point -> Bunch Point
surgery (Bunch (nx:nxs)) (Bunch (nn:_)) = Bunch $ butcher nx nn:nxs

butcher :: Point -> Point -> Point
butcher (Pointy _ y) (Pointy nx _) = Pointy nx y

