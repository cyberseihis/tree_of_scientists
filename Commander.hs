{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commander where

import Kdtree
import Data.Char (ord)
import Control.Arrow ((>>>), Arrow ((&&&), (***), second))
import KdTest (GeneralTree(make, querry))
import Test.QuickCheck (Arbitrary (arbitrary), choose)
import Data.List (sort)
import LshMinhash

alphaOrd = fromIntegral . (- ord 'A') . ord

data Query = Query
    {letterMin :: Char
    ,letterMax :: Char
    ,prizesMin :: Double
    ,prizesMax :: Double} deriving (Eq,Show)

type IPair = (Index,Index)

queryPoints :: Query -> (Point,Point)
queryPoints Query {..} =
    (Pointx (alphaOrd letterMin) prizesMin [], Pointx (alphaOrd letterMax) prizesMax [])

execution :: GeneralTree a => a -> Query -> [Index]
execution = curry $
    concatMap stuff . (uncurry.flip.uncurry) querry.second queryPoints

candidates :: [[Band]] -> [Index] -> [IPair]
candidates bmatrix indices =
    map reindex . matrixToPairs . map (bmatrix!!) $ indices
    where reindex = reid *** reid
          reid = (indices!!)
