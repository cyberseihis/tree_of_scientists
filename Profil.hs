module Profil where

import Dumped (ktree,qtree,twotree,rtree)
import Commander (execution,Query)
import Test.QuickCheck (quickCheck)
import KdTest (GeneralTree)

sampleQuery :: GeneralTree a => a -> Query -> Bool
sampleQuery tree query = execution tree query `seq` True

krun = quickCheck $ sampleQuery ktree
qrun = quickCheck $ sampleQuery qtree
trun = quickCheck $ sampleQuery twotree
rrun = quickCheck $ sampleQuery rtree

go = sequence_ [krun,qrun,trun,rrun]
