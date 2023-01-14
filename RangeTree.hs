{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

type Bunch a = [a]

matchUp :: Ord a => [a] -> [Bunch a]
matchUp = sort >>> group

makeRang :: Ord a => [a] -> OneTree (Bunch a)
makeRang = makeOne . matchUp

rangeRang low high = rangeOne ll hh where
        ll = [low]
        hh = [high]

makeOne [] = Onempty
makeOne [x] = Oleaf x
makeOne xs =
    Onode median mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeOne (median:less)
    mkMore = makeOne more

rangeOne :: (Reportable a,Ord a) => a-> a-> OneTree a -> [a]
rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = [n | low <=n && n<=high]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeLeft low high less ++ rangeRight low high more
    | low <= n = rangeOne low high less
    | otherwise = rangeOne low high more

rangeLeft ::
    (Reportable a, Ord a) =>
    a -> a-> OneTree a -> [a]
rangeLeft _ _ Onempty = []
rangeLeft low high (Oleaf n) = [n | low <= n]
rangeLeft low high Onode {..}
    | low > n = rangeLeft low high more
    | otherwise = reportx more ++ rangeLeft low high less
    where
    reportx = reportO low high

rangeRight _ _ Onempty = []
rangeRight low high (Oleaf n) = [n | high > n]
rangeRight low high Onode {..}
    | high <= n = rangeRight low high less
    | otherwise = reportx less ++ rangeRight low high more
    where
    reportx = reportO low high

subRange ::
    Obp -> Obp ->
    OneTree Obp -> [Obp]
subRange _ _ Onempty = []
subRange (Oleaf low) (Oleaf high) (Oleaf x) =
    map Oleaf $ rangeOne (map other low) (map other high) x
subRange (Oleaf low) (Oleaf high) x =
    let llow = map other low
        hhigh = map other high
        ff = rangeOne llow hhigh
        ggg = ff $ n x
    in map Oleaf ggg

type Obp = OneTree (Bunch Point)

class Reportable a where
    reportO :: a -> a -> OneTree a -> [a]
    reportO _ _ = report

instance Reportable a

instance {-# OVERLAPPING #-} Reportable Obp where
    reportO = subRange

report Onempty = []
report (Oleaf n) = [n]
report Onode {..} = concatMap report [less,more]

isSplitnode low high Onode {n} = low <= n && n <= high

sprout :: Obp -> OneTree Obp
sprout Onempty = Onempty
sprout x@(Oleaf a) = Oleaf x
sprout x@Onode {..} = Onode {n=sideTree n.report$x, less=sprout less, more=sprout more}

-- Previous point of comparison given to change root node to compare well
-- in both dimentions
sideTree :: Bunch Point -> [Bunch Point] -> OneTree (Bunch Point)
sideTree nn points =
    let x@Onode {..} = makeRang . map other . concat $ points
        y = x {n=surgery n nn}
    in y

surgery :: Bunch Point -> Bunch Point -> Bunch Point
surgery ((nx:nxs)) ((nn:_)) = butcher nx nn:nxs

butcher :: Point -> Point -> Point
butcher (Pointy _ y) (Pointx nx _) = Pointy nx y

