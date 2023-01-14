{-# LANGUAGE LexicalNegation #-}
module Kdtree  where
import Data.List (sortOn, sort)
import Control.Arrow (Arrow((&&&)), (>>>))

type Axis = Bool
data Kd = Kempty | Kd Point Kd Kd deriving (Eq, Show)

data Point = Pointx {x :: Int, y :: Int} | Pointy {x :: Int, y :: Int} deriving (Eq, Show)

instance Ord Point where
    (Pointx x _) <= (Pointx z _) = x <= z
    (Pointx x _) <= (Pointy z _) = x <= z
    (Pointy _ y) <= (Pointy _ z) = y <= z
    (Pointy _ y) <= (Pointx _ z) = y <= z

other (Pointx x y) = Pointy x y
other (Pointy x y) = Pointx x y

makeKd [] = Kempty
makeKd points = Kd median (subKd less) (subKd more)
  where
    (less, median, more) = splitApart points
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
splitDownHalf = uncurry splitAt . (divUpHalf.length &&& id)

divUpHalf = max 0 . (- 1) . (`div`2)

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

