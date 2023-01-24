{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commander where

import Kdtree
import Data.Char (ord)
import Control.Arrow ((>>>), Arrow ((&&&)))
import KdTest (GeneralTree(make, querry))
import Test.QuickCheck (Arbitrary (arbitrary), choose)
import Data.List (sort)

------------------------------------------------------------
-- Constants

main = do
    spreadSheet <- lines <$> readFile "sorteddata.csv"
    let space = csvToPoints spreadSheet
        kdtree :: Kd = make space
        qq = Query 'G' 'M' 1 9 0
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
    ,prizesMax :: Int
    ,threshold :: Float} deriving (Eq,Show)

instance Arbitrary Query where
    arbitrary = do
        ls <- choose ('A','Z')
        lb <- choose (ls,'Z')
        ps <- choose (0,13)
        pb <- choose (ps,13)
        thr <- choose (0,1)
        return $ Query ls lb ps pb thr

execution :: GeneralTree a => a -> Query -> [Int]
execution tree Query {..} =
    let low = Pointx (alphaOrd letterMin) prizesMin 0
        high = Pointx (alphaOrd letterMax) prizesMax 0
    in stuff <$> querry low high tree
