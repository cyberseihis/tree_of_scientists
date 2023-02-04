{-# LANGUAGE RecordWildCards #-}
module RangeTree (TwoTree (..),makeRange,rangeRange) where
import Kdtree
import Control.Arrow ((>>>))
import Data.List (sort, group, partition, singleton, groupBy, sortOn)
import Data.Maybe (mapMaybe)

data TwoTree =
    Tnempty | Tleaf { tn :: Bunch, tside :: TwoTree} |
    Tnode {
        tn :: Bunch,
        tside :: TwoTree,
        tless :: TwoTree,
        tmore :: TwoTree} deriving (Eq,Show)

newtype Bunch = Bunch {unBunch:: [Point]} deriving (Show)

instance Eq Bunch where
    Bunch (x:_) == Bunch (y:_) = x <= y && x >= y
    
instance Ord Bunch where
    Bunch (x:_) <= Bunch (y:_) = x <= y

makeRange :: [Point] -> TwoTree
makeRange = matchUp x >>> makeTwo

rangeRange :: Point -> Point -> TwoTree -> [Point]
rangeRange low high =
    concatMap unBunch . rangeTwo (Bunch [low]) (Bunch [high]) Idk

bother :: Bunch -> Bunch
bother = Bunch . map other . unBunch

matchUp x = sortOn x >>> groupBy (\pa pb->x pa == x pb) >>> map Bunch

makeTwo [] = Tnempty
makeTwo [x] = Tleaf x (makeSide [x])
makeTwo xs =
    Tnode median (makeSide xs) mkLess mkMore where
    (less, median, more) = splitEarly xs
    mkLess = makeTwo (median:less)
    mkMore = makeTwo more

makeSide :: [Bunch] -> TwoTree
makeSide = makeTwo . matchUp y . mapMaybe sother . concatMap unBunch
    where sother p@Pointx {} = Just $ other p
          sother _ = Nothing

data Farse = Idk | Lft | Rgt | Dwn deriving (Eq)

rangeTwo :: Bunch -> Bunch -> Farse -> TwoTree -> [Bunch]
rangeTwo _ _ _ Tnempty = []
rangeTwo low@(Bunch(Pointx {}:_)) high Dwn t =
    rangeTwo (bother low) (bother high) Dwn . tside $ t
rangeTwo low high Dwn Tleaf {..} =[tn|(low <= tn) && (high >= tn)]
rangeTwo low high _ Tleaf {..} =
    if (low <= tn) && (high >= tn)
    then rangeTwo (bother low) (bother high) Dwn tside
    else []
rangeTwo low high lor Tnode {..}
    | high <= tn = rangeTwo low high lor tless
    | low > tn   = rangeTwo low high lor tmore
    | lor == Dwn = rangeTwo low high lor tless ++ rangeTwo low high lor tmore
    | lor == Idk = rangeTwo low high Lft tless ++ rangeTwo low high Rgt tmore
    | lor == Rgt = rangeTwo low high Dwn tless ++ rangeTwo low high lor tmore
    | otherwise  = rangeTwo low high Dwn tmore ++ rangeTwo low high lor tless
