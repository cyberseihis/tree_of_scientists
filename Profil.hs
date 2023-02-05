{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Test.QuickCheck (quickCheck, withMaxSuccess, Args (maxSuccess, chatty), quickCheckWith, stdArgs, generate, Arbitrary (arbitrary))
import KdTest (GeneralTree (make, querry))
import Control.Monad (replicateM)
import Kdtree
import Criterion.Main (defaultMain, bench, nfIO)
import Control.DeepSeq (deepseq)

sampleQuery :: GeneralTree a => a -> Point -> Point -> Bool
sampleQuery tree p1 p2 = map stuff (querry p1 p2 tree) `deepseq` True

heatCPU tree = quickCheckWith stdArgs {maxSuccess=10000,chatty=False} $ sampleQuery tree

main = do
    points <- replicateM 100 (generate arbitrary):: IO [Point]
    let
        ktree :: Kd = make points
    defaultMain [
        bench "trees" $ nfIO (heatCPU ktree)
        ]
