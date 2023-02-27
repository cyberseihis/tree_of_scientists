{-# LANGUAGE RecordWildCards #-}
module Muhtr where
import Dumped (theBands,wordSets,prettys)
import Kdtree
import Data.List (sort, singleton)
import LshMinhash (sanityJacc)
import Control.Arrow (Arrow((***)))
import Commander (alphaOrd)
import Quad 
import RangeTree 
import RTree 
import KdTest (GeneralTree(make))
import Aesoning
import Data.Aeson
import RTree (Mbr(Mbr))

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

stm = Mbr 0 0 25 13

grKd (Mbr {..}) (Pointx {..}) = [Mbr sx sy x my,Mbr x sy mx my]
grKd (Mbr {..}) (Pointy {..}) = [Mbr sx sy mx y,Mbr sx y mx my]

grQu (Mbr {..}) (Pointx {..}) = [Mbr x y mx my,Mbr x sy mx y,Mbr sx sy x y,Mbr sx y x my]

tupl Mbr{..} = (sx,sy,mx,my)
ptup Pointx{..} = (x,y)
ptup Pointy{..} = (x,y)

recKd mb (Kd p a b) =
    let [l,r] = grKd mb p
    in [l,r] ++ recKd l a ++ recKd r b
recKd _ _ = []

recQu mb Qtree {..} =
    let mdi@[mne,mse,msw,mnw] = grQu mb m
    in mdi ++ concat (zipWith recQu mdi [sw,se,ne,nw])
recQu _ _ = []

recRa mb Tnode {..} =
    let [l,r] = grKd mb tn
    in [l,r] ++ recRa l tless ++ recRa r tmore ++ recRa mb tside
recRa _ _ = []

recRT (Rnode mb children) = mb:concatMap recRT children
recRT _ = []
