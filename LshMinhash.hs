{-# LANGUAGE TupleSections #-}
module LshMinhash where
import Control.Arrow ((>>>), Arrow ((***), (&&&)))
import GHC.Integer (hashInteger)
import Data.Hash
import Data.List hiding (union)
import Data.List.Split
import Data.Set (fromList, intersection, size, union)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

jaccardSimilarity xs ys =
    let setX = fromList . words $ xs
        setY = fromList . words $ ys
    in  fromIntegral (size (intersection setX setY)) / fromIntegral (size (setX `union` setY))

docHash :: String -> Int -> Int
docHash str i = minimum . map f . words $ str
    where f = easyHash . (i,) . easyHash

hash2Int :: Hash -> Int
hash2Int = fromIntegral . asWord64 . hash . asWord64

easyHash :: Hashable a => a -> Int
easyHash = hash2Int . hash

someNumbers = easyHash <$> [(1::Int)..]

license str =
    take 100 $ docHash str <$> someNumbers

sigSimilarity x y = (/100) . fromIntegral . length . filter id $ zipWith (==) (license x) (license y)
sigKinda x y = (/100) . fromIntegral . length . filter id $ zipWith (==) x y

type Band = Int
type Index = Int
type Bucket = Map Index [Index]

-- Number of rows/bands hardcoded for 60%
bandifyX :: [Int] -> [Band]
bandifyX = chunksOf 5 >>> map easyHash

-- No set ammount of buckets, just a map
getBuckets :: [Band] -> Bucket
getBuckets = 
    zip [(0::Index)..] >>>
    sortOn snd >>>
    groupBy (\(_,x) (_,y)->x==y) >>>
    map (snd.head &&& map fst) >>>
    Map.fromList

finalle = map license >>> map bandifyX >>> transpose >>> map getBuckets
