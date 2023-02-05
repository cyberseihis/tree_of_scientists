{-# LANGUAGE LexicalNegation #-}
module Kdtree  where
import Data.List (sortOn, sort, partition)
import Control.Arrow (Arrow((&&&)), (>>>))

data Kd = Kempty | Kleaf Point | Kd Point Kd Kd deriving (Eq, Show)

data Point = Pointx {x :: Double, y :: Double, stuff :: [Int]} | Pointy {x :: Double, y :: Double, stuff :: [Int]} deriving (Eq, Show)

instance Ord Point where
    (Pointx x _ _) `compare` (Pointx z _ _) = x `compare` z
    (Pointy _ y _) `compare` (Pointy _ z _) = y `compare` z

other (Pointx x y i) = Pointy x y i
other (Pointy x y i) = Pointx x y i

makeKd [] = Kempty
makeKd [x] = Kleaf x
makeKd points = Kd mid (subKd less) (subKd more)
  where mid = meanp points
        (less,more) = partition (<=mid) points
        subKd = makeKd . map other

rangeKd :: Point -> Point -> Kd -> [Point]
rangeKd _ _ Kempty = []
rangeKd low high (Kleaf x) = [x | isCovered low high x]
rangeKd low high (Kd point kd1 kd2) =
    let nextQuerry = rangeKd (other low) (other high)
        h = [kd2|point <= high] ++ [kd1|low <= point]
    in concatMap nextQuerry h

isCovered low high point =
    low <= point && point <= high &&
    other low <= other point && other point <= other high

meanp :: [Point] -> Point
meanp xs =
    let n = fromIntegral $ length xs
        mx = (/n).sum.map x $ xs
        my = (/n).sum.map y $ xs
        pt Pointx {} = Pointx
        pt Pointy {} = Pointy
    in pt (head xs) mx my []
