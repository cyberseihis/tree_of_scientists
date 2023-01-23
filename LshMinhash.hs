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

docHash :: String -> Hash -> Int
docHash str i = minimum . map f . words $ str
    where f = hash2Int . combine i . hash

hash2Int :: Hash -> Int
hash2Int = fromIntegral . asWord64 . hash . asWord64

easyHash :: Hashable a => a -> Int
easyHash = hash2Int . hash

someNumbers = hash <$> [(1::Int)..]

license str =
    take 120 $ docHash str <$> someNumbers

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
