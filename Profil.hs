module Main where

import Dumped (ktree,qtree,twotree,rtree)
import Commander (execution,Query)
import Test.QuickCheck (quickCheck, withMaxSuccess, Args (maxSuccess, chatty), quickCheckWith, stdArgs)
import KdTest (GeneralTree)

sampleQuery :: GeneralTree a => a -> Query -> Bool
sampleQuery tree query = execution tree query `seq` True

heatCPU tree = quickCheckWith stdArgs {maxSuccess=10000,chatty=False} $ sampleQuery tree

krun = {-# SCC "KTREE" #-} heatCPU ktree
qrun = {-# SCC "QUADTREE" #-} heatCPU qtree
trun = {-# SCC "RANGETREE" #-} heatCPU twotree
rrun = {-# SCC "R-TREE" #-} heatCPU rtree

main = sequence_ [krun,qrun,trun,rrun]
