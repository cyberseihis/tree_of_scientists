{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE UndecidableInstances #-}
module RangeTree where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group, partition, singleton, groupBy, sortOn)

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

matchUp = sort >>> groupBy (\pa pb->x pa == x pb)
matchUpy = sortOn y >>> groupBy (\pa pb->y pa == y pb)

makeRang = sprout . makeOne . matchUp

rangeRang :: Point -> Point -> OneTree Obp -> [Point]
rangeRang low high =
    concatMap (\(Oleaf x)->x) . rangeOne ll hh where
        ll = Oleaf [low]
        hh = Oleaf [high]

makeOne [] = Onempty
makeOne [x] = Oleaf x
makeOne xs =
    Onode median mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeOne (median:less)
    mkMore = makeOne more

looksBounded low high n =
    compare low n /= GT &&
    compare high n /= LT

rangeOne :: (Reportable a,Ord a) => a-> a-> OneTree a -> [a]
rangeOne _ _ Onempty = []
rangeOne low high (Oleaf n) = doubleCheck low high 
    [n|looksBounded low high n]
rangeOne low high o@(Onode {..})
    | isSplitnode low high o =
        rangeLeft low high less ++ rangeRight low high more
    | compare low n /= GT = rangeOne low high less
    | otherwise = rangeOne low high more

rangeLeft ::
    (Reportable a, Ord a) =>
    a -> a-> OneTree a -> [a]
rangeLeft _ _ Onempty = []
rangeLeft low high (Oleaf n) = doubleCheck low high 
    [n | compare low n /= GT]
rangeLeft low high Onode {..}
    | compare low n == GT= rangeLeft low high more
    | otherwise = reportx more ++ rangeLeft low high less
    where
    reportx = reportO low high

rangeRight _ _ Onempty = []
rangeRight low high (Oleaf n) = doubleCheck low high 
    [n |compare high n /= LT]
rangeRight low high Onode {..}
    | compare high n /= GT  = rangeRight low high less
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

class Ord a => Reportable a where
    reportO :: a -> a -> OneTree a -> [a]
    reportO _ _ = report
    doubleCheck :: a -> a -> [a] -> [a]
    doubleCheck _ _ = id

instance Ord a => Reportable a

instance {-# OVERLAPPING #-} Reportable Obp where
    reportO = subRange
    doubleCheck _ _ [] = []
    doubleCheck l h [olf] = subRange l h $ Oleaf olf

report Onempty = []
report (Oleaf n) = [n]
report Onode {..} = concatMap report [less,more]

isSplitnode low high Onode {n} = looksBounded low high n

sprout :: Obp -> OneTree Obp
sprout Onempty = Onempty
sprout x@(Oleaf a) = Oleaf . sideTree a . matchUpy $ a
sprout x@Onode {..} = Onode {n=sideTree n.report$x, less=sprout less, more=sprout more}

-- Previous point of comparison given to change root node to compare well
-- in both dimentions
sideTree :: Bunch Point -> [Bunch Point] -> Obp
sideTree nn points =
    let x = makeOne . matchUp . map other . concat $ points
    in case x of
        Onode {..} -> x {n=surgery n nn}
        Oleaf xx -> Oleaf $ surgery xx nn

surgery :: Bunch Point -> Bunch Point -> Bunch Point
surgery ((nx:nxs)) ((nn:_)) = butcher nx nn:nxs

butcher :: Point -> Point -> Point
butcher (Pointy _ y) (Pointx nx _) = Pointy nx y

