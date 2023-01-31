{-# LANGUAGE LexicalNegation #-}
module Kdtree  where
import Data.List (sortOn, sort)
import Control.Arrow (Arrow((&&&)), (>>>))

data Kd = Kempty | Kd Point Kd Kd deriving (Eq, Show)

data Point = Pointx {x :: Double, y :: Double, stuff :: [Int]} | Pointy {x :: Double, y :: Double, stuff :: [Int]} deriving (Eq, Show)

instance Ord Point where
    (Pointx x _ _) `compare` (Pointx z _ _) = x `compare` z
    (Pointy _ y _) `compare` (Pointy _ z _) = y `compare` z

other (Pointx x y i) = Pointy x y i
other (Pointy x y i) = Pointx x y i

makeKd [] = Kempty
makeKd points = Kd median (subKd less) (subKd more)
  where (less, median, more) = splitApart points
        subKd = makeKd . map other

rangeKd :: Point -> Point -> Kd -> [Point]
rangeKd _ _ Kempty = []
rangeKd lowp highp (Kd point kd1 kd2) =
    let nextQuerry = rangeKd (other lowp) (other highp)
        above = if point <= highp then nextQuerry kd2 else []
        below = if lowp <= point then nextQuerry kd1 else []
        mid = [point | isCovered lowp highp point]
    in concat [below, mid, above]

isCovered low high point =
    low <= point && point <= high &&
    other low <= other point && other point <= other high

splitInHalf :: [a] -> ([a], [a])
splitInHalf = uncurry splitAt . ((`div`2).length &&& id)

splitDownHalf :: [a] -> ([a], [a])
splitDownHalf = uncurry splitAt . (max 0.(- 1).(`div`2).length &&& id)

splitApart xs =
    let
    (below, median : above) = splitInHalf . sort $ xs
    (less,more) = break (>median) (below++above)
    in (less,median,more)

splitEarly xs =
    let
    (below, median : above) = splitDownHalf . sort $ xs
    (less,more) = break (>median) (below++above)
    in (less,median,more)
