module EzHash where

import LshMinhash
import Data.List (group, sort)
import Control.Arrow ((&&&))

educate = unwords . drop 2 . words 
stories = map educate . lines
rephrase = show . map license . stories

isMinOk x y =
    let gold = jaccardSimilarity x y > 0.6
        dut = sigSimilarity x y > 0.6
    in (dut,gold)

minHashTest xs =
    let ys = [isMinOk x y|x<-xs,y<-xs,x/=y]
        ls = group . sort $ ys
        nx = map (head &&& length) ls
    in nx

sortData = readFile "sorteddata.csv"

main = do
    inp <- sortData
    writeFile "myHashes.csv" (rephrase inp)
