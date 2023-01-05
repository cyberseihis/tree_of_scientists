module Quad where
import Kdtree
import Data.List.NonEmpty (groupAllWith, toList)
import Data.Map.Strict ( Map )
import Control.Arrow (Arrow((&&&), first, second), (>>>))
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

data Qtree = Qempty | Qtree Point (Map Quad Qtree) deriving (Eq,Show)
type Quad = (Bool,Bool)

compQuarts x y = (x <= y, other x <= other y)

emptyQuart = Map.fromList . zip [(x,y)|x<-bls,y<-bls] $ repeat []
    where bls = [False,True]

splitAround x =
    map (compQuarts x &&& id) >>>
    groupAllWith fst >>>
    map (toList >>> unzip >>> first head) >>>
    Map.fromList >>>
    (`Map.union` emptyQuart)

spreadAround x =
    splitAround x >>>
    Map.elems >>>
    map length >>>
    (maximum &&& minimum) >>>
    uncurry (-)

makeQtree :: [Point] -> Qtree
makeQtree [] = Qempty
makeQtree points = mkQ points
mkQ =
    removeEach >>>
    maximumBy (comparing $ uncurry spreadAround) >>>
    (fst &&& uncurry splitAround) >>>
    second (Map.map makeQtree) >>>
    uncurry Qtree

removeEach xs = zip xs (map (`List.delete` xs) xs)
