{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commander where

import Kdtree
import Quad
import RTree
import RangeTree
import Data.Char (ord)
import Control.Arrow ((>>>), Arrow ((&&&), (***), second))
import KdTest (GeneralTree(make, querry))
import Test.QuickCheck (Arbitrary (arbitrary), choose)
import Data.List (sort)
import LshMinhash

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

queryPoints :: Query -> (Point,Point)
queryPoints Query {..} =
    (Pointx (alphaOrd letterMin) prizesMin 0, Pointx (alphaOrd letterMax) prizesMax 0)

execution :: GeneralTree a => a -> Query -> [Index]
execution = curry $
    map stuff . (uncurry.flip.uncurry) querry.second queryPoints

candidates :: [[Band]] -> [Index] -> [IPair]
candidates bmatrix indices =
    map reindex . matrixToPairs . map (bmatrix!!) $ indices
    where reindex = reid *** reid
          reid = (indices!!)
