{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commander where

import Kdtree
import Quad
import RTree
import RangeTree
import Data.Char (ord)
import Control.Arrow ((>>>), Arrow ((&&&), (***)))
import KdTest (GeneralTree(make, querry))
import Test.QuickCheck (Arbitrary (arbitrary), choose)
import Data.List (sort)
import LshMinhash

------------------------------------------------------------
-- Hardcoding constants

dumpTrees = do
    spreadSheet <- lines <$> readFile "sorteddata.csv"
    let space = csvToPoints spreadSheet
        kdtree :: Kd = make space
        quatree :: Qtree = make space
        twotree :: TwoTree = make space
        rtree :: RTree = make space
    writeFile "dumpedTrees.csv" . show $ kdtree
    appendFile "dumpedTrees.csv" . show $ quatree
    appendFile "dumpedTrees.csv" . show $ twotree
    appendFile "dumpedTrees.csv" . show $ rtree

main = do
    spreadSheet <- lines <$> readFile "sorteddata.csv"
    let space = csvToPoints spreadSheet
        kdtree :: Kd = make space
        qq = Query 'G' 'M' 1 9
        uhu = (spreadSheet!!) <$> execution kdtree qq
    mapM_ putStrLn uhu


blurbToPoint :: String ->  (Int,Int)
blurbToPoint blurb =
    let ((leter:_):awa:_) = words blurb
    in (alphaOrd leter,read awa)

blrb2 :: String ->  (Int,Int)
blrb2 = words >>>
    (alphaOrd.head.head &&& read.head.tail)

csvToPoints :: [String] -> [Point]
csvToPoints = map blurbToPoint >>> zip [0..] >>>
     map (\(i,(x,y)) -> Pointx x y i)

alphaOrd = (- ord 'A') . ord

data Query = Query
    {letterMin :: Char
    ,letterMax :: Char
    ,prizesMin :: Int
    ,prizesMax :: Int} deriving (Eq,Show)

type IPair = (Index,Index)

instance Arbitrary Query where
    arbitrary = do
        ls <- choose ('A','Z')
        lb <- choose (ls,'Z')
        ps <- choose (0,13)
        pb <- choose (ps,13)
        return $ Query ls lb ps pb

execution :: GeneralTree a => a -> Query -> [Index]
execution tree Query {..} =
    let low = Pointx (alphaOrd letterMin) prizesMin 0
        high = Pointx (alphaOrd letterMax) prizesMax 0
    in stuff <$> querry low high tree

candidates :: [[Band]] -> [Index] -> [IPair]
candidates bmatrix indices =
    map reindex . matrixToPairs . map (bmatrix!!) $ indices
    where reindex = reid *** reid
          reid = (indices!!)


everyPair :: [Index] -> [IPair]
everyPair = groupsToPairs
