module KdTest where
import Kdtree
import Test.QuickCheck

instance Arbitrary Point where
    arbitrary = do
        rx <- arbitrary
        Pointx rx <$> arbitrary

instance Arbitrary Kd where
    arbitrary = do
        makeKd <$> arbitrary

prop_all_in pl ph kdt = smaller pl ph ==>
    all (isCovered pl ph) . map normaliseToX $ rangeKd pl ph kdt

smaller :: Point -> Point -> Bool
smaller pl ph = pl <= ph && other pl <= other ph

normaliseToX (Pointy x y) = Pointx x y
normaliseToX p = p

-- >>> quickCheck prop_all_in

