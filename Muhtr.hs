module Muhtr where
import Dumped (theBands,wordSets,prettys)
import Kdtree
import Data.List (sort, singleton)
import LshMinhash (sanityJacc)
import Control.Arrow (Arrow((***)))
import Commander (alphaOrd)
import Quad (makeQtree, Qtree (Qtree))
import RangeTree (makeRange, TwoTree)
import RTree (makeRtree, RTree)
import KdTest (GeneralTree(make))
import Aesoning
import Data.Aeson

poinify = (\[c:_,i]->Pointx (alphaOrd c) (read i).singleton).words
ktree = make points :: Kd
qtree = make points :: Qtree
ratree = make points :: TwoTree
rtree = make points :: RTree
points = zipWith poinify prettys [0..]

kjson = toJSON ktree

chrome = do
    encodeFile "kdjs.json" ktree
    encodeFile "quadjs.json" qtree
    encodeFile "rangjs.json" ratree
    encodeFile "rtrjs.json" rtree
