module EzHash where

import LshMinhash

educate = unwords . drop 2 . words 
stories = map educate . lines
rephrase = show . map license . stories


main = do
    inp <- readFile "sorteddata.csv"
    writeFile "myHashes.csv" (rephrase inp)
