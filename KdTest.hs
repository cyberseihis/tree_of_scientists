{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module KdTest where
import Kdtree
import RangeTree
import RTree
import Test.QuickCheck
import Data.List ((\\))
import Quad
import Debug.Trace (traceShow)

instance GeneralTree Kd where
    make = makeKd
    querry = rangeKd

instance GeneralTree Qtree where
    make = makeQtree
    querry = rangeQt

instance GeneralTree TwoTree where
    make = makeRange
    querry = rangeRange

instance GeneralTree RTree where
    make = makeRtree 10
    querry = rangeRt

instance Arbitrary Point where
    arbitrary = do
        Pointx <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Kd where
    arbitrary = do
        make <$> arbitrary

instance Arbitrary Qtree where
    arbitrary = do
        make <$> arbitrary



class GeneralTree a where
    make :: [Point] -> a
    emptyTree :: a
    emptyTree = make []
    querry :: Point -> Point -> a -> [Point]
    prop_all_in :: a -> Point -> Point -> [Point] -> Property
    prop_all_in _ pl ph xs = smaller pl ph ==>
        all (isCovered pl ph . normaliseToX) . querry pl ph $ (make xs::a)
    prop_all_minmax :: a -> [Point] -> Bool
    prop_all_minmax  _ xs =
        let minPoint = Pointx minBound minBound 0
            maxPoint = Pointx maxBound maxBound 0
            kdt :: a= make xs
            ys = map normaliseToX $ querry minPoint maxPoint kdt
        in samesame xs ys

    prop_model_filter :: a -> Point -> Point -> [Point] -> Property
    prop_model_filter _ pl ph xs = smaller pl ph ==>
        let ys = map normaliseToX . querry pl ph $ (make xs::a)
            zs = filter (isCovered pl ph) xs
        in samesame ys zs

    prop_only_self :: a -> [Point] -> Property
    prop_only_self _ xs = not (null xs) ==>
        let x = head xs
            ys = map normaliseToX . querry x x $ (make xs::a)
        in not (null ys) && all (==x) ys

tsts :: GeneralTree a => a -> IO ()
tsts x = do
    quickCheck (prop_only_self x)
    quickCheck (prop_all_in x)
    quickCheck (prop_all_minmax x)
    quickCheck (prop_model_filter x)

samesame xs ys = null (xs \\ ys) && null (ys \\ xs)

smaller :: Point -> Point -> Bool
smaller pl ph = pl <= ph && other pl <= other ph

normaliseToX (Pointy x y i) = Pointx x y i
normaliseToX p = p
