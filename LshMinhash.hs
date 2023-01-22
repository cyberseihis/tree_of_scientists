module LshMinhash where
import Control.Arrow ((>>>), Arrow ((***)))
import GHC.Integer (hashInteger)
import Data.Hash
import Data.List.Split

idk :: String -> Hash -> Int
idk str i = minimum . map f . words $ str
    where f = fromIntegral . asWord64 . hash . asWord64 . combine i . hash

someNumbers = hash <$> [(1::Int)..]

license str = 
    take 120 $ idk str <$> someNumbers
