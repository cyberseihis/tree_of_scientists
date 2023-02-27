{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE DeriveGeneric #-}
module RangeTree (TwoTree (..),makeRange,rangeRange) where
import Kdtree
import Data.List (nub,partition)
import Debug.Trace (traceShow, trace)
import GHC.Generics

data TwoTree =
    Tnempty | Tleaf { tn :: Point, tdata :: [Point], tside :: TwoTree} |
    Tnode { tn :: Point, tside :: TwoTree, tless :: TwoTree,
        tmore :: TwoTree} deriving (Eq,Show,Generic)

rangeRange :: Point -> Point -> TwoTree -> [Point]
rangeRange low high = rangeTwo low high Idk


isK xs@(Pointy {}:_) = (==1) . length . nub . map y $ xs
isK xs = (==1) . length . nub . map x $ xs

makeRange :: [Point] -> TwoTree
makeRange [] = Tnempty
makeRange xs
    | isK xs = Tleaf (head xs) xs (makeSide xs)
    | otherwise =
    Tnode mid (makeSide xs) mkLess mkMore where
    mid = meanp xs
    (less,more) = partition (<=mid) xs
    mkLess = makeRange less
    mkMore = makeRange more

makeSide :: [Point] -> TwoTree
makeSide = makeRange . map other . filter isx
    where isx Pointx {} = True
          isx _ = False 

data Farse = Idk | Lft | Rgt | Dwn deriving (Eq)

rangeTwo :: Point -> Point -> Farse -> TwoTree -> [Point]
rangeTwo _ _ _ Tnempty = []
rangeTwo low@Pointx {} high Dwn t =
    rangeTwo (other low) (other high) Dwn . tside $ t
rangeTwo low high Dwn Tleaf {..} = concat[tdata|(low <= tn) && (high >= tn)]
rangeTwo low high _ Tleaf {..} =
    if (low <= tn) && (high >= tn)
    then rangeTwo (other low) (other high) Dwn tside
    else []
rangeTwo low high lor Tnode {..}
    | high <= tn = rangeTwo low high lor tless
    | low > tn   = rangeTwo low high lor tmore
    | lor == Dwn = rangeTwo low high lor tless ++ rangeTwo low high lor tmore
    | lor == Idk = rangeTwo low high Lft tless ++ rangeTwo low high Rgt tmore
    | lor == Rgt = rangeTwo low high Dwn tless ++ rangeTwo low high lor tmore
    | otherwise  = rangeTwo low high Dwn tmore ++ rangeTwo low high lor tless
