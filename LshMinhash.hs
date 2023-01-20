module LshMinhsash where
import Control.Arrow ((>>>), Arrow ((***)))
import GHC.Integer (hashInteger)
import Data.Hashable
import Data.List.Split

type WordH = Int
type Doc = [WordH]
type HashFunc = WordH -> Int

docHash :: String -> Doc
docHash = words >>> map hash

docMinHash :: Doc -> HashFunc -> Int
docMinHash doc hf = minimum . map hf $ doc

someHashes :: [HashFunc]
someHashes = hashWithSalt <$> [1..]

docIdVector :: Int -> Doc -> [Int]
docIdVector n doc = docMinHash doc <$> take n someHashes

bucketHash :: Int -> [Int] -> [Int]
bucketHash n = chunksOf n >>> map hash
