module Profil where

import Dumped (ktree,qtree,twotree,rtree)
import Commander (execution,Query)
import Test.QuickCheck (quickCheck, withMaxSuccess, Args (maxSuccess, chatty), quickCheckWith, stdArgs)
import KdTest (GeneralTree)

sampleQuery :: GeneralTree a => a -> Query -> Bool
sampleQuery tree query = execution tree query `seq` True

heatCPU tree = quickCheckWith stdArgs {maxSuccess=100000,chatty=False} $ sampleQuery tree

krun = heatCPU ktree
qrun = heatCPU qtree
trun = heatCPU twotree
rrun = heatCPU rtree

go = sequence_ [krun,qrun,trun,rrun]
