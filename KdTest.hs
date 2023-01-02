module KdTest where
import Kdtree
import Test.QuickCheck
import Data.List ((\\))

instance Arbitrary Point where
    arbitrary = do
        rx <- arbitrary
        Pointx rx <$> arbitrary

instance Arbitrary Kd where
    arbitrary = do
        makeKd <$> arbitrary

prop_all_in pl ph kdt = smaller pl ph ==>
    all (isCovered pl ph . normaliseToX) $ rangeKd pl ph kdt

prop_all_minmax xs =
    let minPoint = Pointx minBound minBound
        maxPoint = Pointx maxBound maxBound
        kdt = makeKd xs
        ys = map normaliseToX $ rangeKd minPoint maxPoint kdt
    in samesame xs ys

samesame xs ys = null (xs \\ ys) && null (ys \\ xs)

smaller :: Point -> Point -> Bool
smaller pl ph = pl <= ph && other pl <= other ph

normaliseToX (Pointy x y) = Pointx x y
normaliseToX p = p
