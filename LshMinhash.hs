{-# LANGUAGE TupleSections #-}
module LshMinhash where
import Control.Arrow ((>>>), Arrow ((***), (&&&)))
import GHC.Integer (hashInteger)
import Data.Hash
import Data.List hiding (union)
import Data.List.Split
import Data.HashMap.Strict (HashMap, alter, empty, mapMaybe, elems)
import Data.IntSet (IntSet, size, intersection, union)

sanityJacc :: IntSet -> IntSet -> Bool
sanityJacc xs ys = fromIntegral (size (intersection xs ys)) / fromIntegral (size (xs `union` ys)) > 0.2

docHash :: String -> Int -> Int
docHash str i = minimum . map f . words $ str
    where f = easyHash . (easyHash i,) . easyHash

easyHash :: Hashable a => a -> Int
easyHash = fromIntegral . asWord64 . hash . asWord64 . hash

license str = docHash str <$> [(1::Int)..100]

type Band = Int
type Index = Int
type Bucket = HashMap Index [Index]

-- Number of rows/bands hardcoded for 20%
bandifyX :: [Int] -> [Band]
bandifyX = chunksOf 2 >>> take 50 >>> map ((`mod`10000).easyHash)

rowToBucket :: [Band] -> Bucket
rowToBucket = zip [0..] >>> foldl' include empty

include :: Bucket -> (Index,Band) -> Bucket
include bucket (i,band) = alter (shoveIndex i) band bucket

shoveIndex :: Index -> Maybe [Index] -> Maybe [Index]
shoveIndex ind  = Just . (ind:) . concat

groupsToPairs :: Ord a => [a] -> [(a,a)]
groupsToPairs xs = [(x,y)|x<-xs,y<-xs,x<y]

matrixToPairs :: [[Band]] -> [(Index,Index)]
matrixToPairs =
    transpose >>>
    concatMap (elems . rowToBucket) >>>
    concatMap groupsToPairs >>>
    nub
