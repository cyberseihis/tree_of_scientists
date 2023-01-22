module Commaner where

import Kdtree
import Data.Char (ord)
import Control.Arrow ((>>>))
import KdTest (GeneralTree(make))
import Test.QuickCheck (Arbitrary (arbitrary), choose)
import Data.List (sort)

blurbToPoint :: String ->  (Int,Int)
blurbToPoint blurb = 
    let ((leter:_):awa:_) = words blurb
        nlet = ord leter - ord 'A'
    in (nlet,read awa)

csvToPoints :: String -> [Point]
csvToPoints = lines >>> map blurbToPoint >>> zip [0..] >>>
     map (\(i,(x,y)) -> Pointx x y i)

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


