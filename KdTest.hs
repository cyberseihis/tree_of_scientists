{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module KdTest where
import Kdtree
import Test.QuickCheck
import Data.List ((\\), singleton)
import Quad
import RangeTree
import RTree
import Control.Monad (liftM3)
import Data.List.NonEmpty (groupAllWith, toList)
import Control.Arrow (Arrow((&&&)))

instance GeneralTree Kd where
    make = makeKd . bundle
    querry = rangeKd

instance GeneralTree Qtree where
    make = makeQtree . bundle
    querry = rangeQt

instance GeneralTree TwoTree where
    make = makeRange . bundle
    querry = rangeRange

instance GeneralTree RTree where
    make = makeRtree 10 . bundle
    querry = rangeRt

instance Arbitrary Point where
    arbitrary = liftM3 Pointx arbitrary arbitrary (singleton <$> arbitrary)

bundle :: [Point] -> [Point]
bundle xs =
    let hm = map toList $ groupAllWith (x&&&y) xs
        ff ps@(p:_) = p {stuff= concatMap stuff ps}
    in map ff hm

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
        let minPoint = Pointx (-1000000) (-1000000) []
            maxPoint = Pointx 1000000 1000000 []
            kdt :: a= make xs
            ys = map normaliseToX $ querry minPoint maxPoint kdt
        in samesame (bundle xs) ys

    prop_model_filter :: a -> Point -> Point -> [Point] -> Property
    prop_model_filter _ pl ph xs = smaller pl ph ==>
        let ys = map normaliseToX . querry pl ph $ (make xs::a)
            zs = filter (isCovered pl ph) (bundle xs)
        in samesame ys zs

tsts :: GeneralTree a => a -> IO ()
tsts x = do
    quickCheck (prop_all_in x)
    quickCheck (prop_all_minmax x)
    quickCheck (prop_model_filter x)

mySuite = do
    tsts Kempty
    tsts Qempty
    tsts Tnempty
    tsts Rempty

samesame xs ys = null (xs \\ ys) && null (ys \\ xs)

smaller :: Point -> Point -> Bool
smaller pl ph = pl <= ph && other pl <= other ph

normaliseToX (Pointy x y i) = Pointx x y i
normaliseToX p = p
