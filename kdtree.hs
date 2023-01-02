import Data.List (sortOn, sort)
import Control.Arrow (Arrow((&&&)))

type Axis = Bool
data Kd = Kempty | Kd Point Kd Kd deriving (Eq, Show)

data Point = Pointx {x :: Int, y :: Int} | Pointy {x :: Int, y :: Int} deriving (Eq, Show)

instance Ord Point where
    (Pointx x _) <= (Pointx z _) = x <= z
    (Pointy _ y) <= (Pointy _ z) = y <= z

other (Pointx x y) = Pointy x y
other (Pointy x y) = Pointx x y

makeKd [] = Kempty
makeKd points = Kd median (subKd less) (subKd more)
  where
    (below, median : above) = splitInHalf . sort $ points
    (less,more) = break (>median) (below++above)
    subKd = makeKd . map other

rangeKd :: Point -> Point -> Kd -> [Point]
rangeKd _ _ Kempty = []
rangeKd lowp highp (Kd point kd1 kd2) =
    let le = lowp <= point
    in undefined
    

xOry :: Bool -> Point -> Int
xOry True = x
xOry False = y

splitInHalf :: [a] -> ([a], [a])
splitInHalf = uncurry splitAt . ((`div`2).length &&& id)
