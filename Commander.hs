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
