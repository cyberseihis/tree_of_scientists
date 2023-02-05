{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Test.QuickCheck (quickCheck, withMaxSuccess, Args (maxSuccess, chatty), quickCheckWith, stdArgs, generate, Arbitrary (arbitrary))
import KdTest (GeneralTree (make, querry))
import Control.Monad (replicateM)
import Kdtree
import Criterion.Main (defaultMain, bench, nfIO, bgroup)
import Control.DeepSeq (deepseq)
import Quad (Qtree(Qtree))
import RangeTree (TwoTree)
import RTree (RTree)

sampleQuery :: GeneralTree a => a -> Point -> Point -> Bool
sampleQuery tree p1 p2 = map stuff (querry p1 p2 tree) `deepseq` True

heatCPU tree = quickCheckWith stdArgs {chatty=False} $ sampleQuery tree

main = do
    points <- replicateM 100 (generate arbitrary):: IO [Point]
    let ktree :: Kd = make points
        qtree :: Qtree = make points
        ratree :: TwoTree = make points
        rtree :: RTree = make points
    defaultMain [ bgroup "trees"
        [bench "Kd" $ nfIO (heatCPU ktree)
        ,bench "Quad" $ nfIO (heatCPU ktree)
        ,bench "Range" $ nfIO (heatCPU ktree)
        ,bench "R" $ nfIO (heatCPU ktree)]]
