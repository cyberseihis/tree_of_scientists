{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Aesoning where
import Kdtree
import Quad
import Data.Aeson
import RangeTree
import RTree

instance ToJSON Point where
    toJSON (Pointx{..}) = object ["X" .= x,"Y".=y]
    toJSON (Pointy{..}) = object ["X" .= x,"Y".=y]

instance ToJSON Kd where
    toJSON Kempty = String "Empty"
    toJSON (Kleaf p) = object ["Leaf".=p]
    toJSON (Kd p l r) = object ["Node".=p,"Left".=l,"Right".=r]

instance ToJSON Qtree where
    toJSON Qempty = String "Empty"
    toJSON (Qleaf p) = object ["Leaf".=p]
    toJSON Qtree{..} = object ["Node".=m,"NorthEast".=ne,"NorthWest".=nw,"SouthEast".=se,"SouthWest".=sw]

instance ToJSON TwoTree where
    toJSON Tnempty = String "Empty"
    toJSON (Tleaf {..}) = object ["Node".=tn,"Data".=tdata,"Side".=tside]
    toJSON Tnode {..} = object ["Node".=tn,"Left".=tless,"Right".=tmore,"Side".=tside]

instance ToJSON RTree where
    toJSON Rempty = String "Empty"
    toJSON (Rleaf p) = object ["Leaf".=p]
    toJSON (Rnode mbr children) = object ["Node".=mbr,"Children".=children]

instance ToJSON Mbr where

