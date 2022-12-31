import Data.List (sortOn)
import Control.Arrow (Arrow((&&&)))

data Kd = Kempty | Kx Point Kd Kd | Ky Point Kd Kd deriving (Eq, Show, Ord)

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show, Ord)

-- Maybe using sets would be more convinient?
-- But how would I take the median
makeKd _ [] = Kempty
makeKd dim points = Kx median (subKd less) (subKd more)
  where
    (below, median : above) = splitInHalf . sortOn axis $ points
    axis = xOry dim
    thres = axis median
    (less,more) = break ((>thres).axis) (below++above)
    subKd = makeKd (not dim)

xOry :: Bool -> Point -> Int
xOry True = x
xOry False = y

splitInHalf :: [a] -> ([a], [a])
splitInHalf = uncurry splitAt . ((`div`2).length &&& id)
