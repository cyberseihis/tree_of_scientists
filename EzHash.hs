module Main where

import LshMinhash
import Data.List (group, sort)
import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Data.List.Split (chunksOf)

educate = unwords . drop 2 . words
stories = map educate . lines
rephrase = show . map license . stories
theBands = show . map (bandifyX.license) . stories

saveMinHashes = 
    readFile "sorteddata.csv" >>=
    writeFile "myHashes.csv" . rephrase

saveLHashes = 
    readFile "sorteddata.csv" >>=
    writeFile "myBands.csv" . theBands
    
-- For r=2 b=50 (which I will be using)
-- [((False,False),4982),((False,True),210),((True,False),3024),((True,True),1096)]

-- Stats of similarities
-- [(.0,1153),(.1,2784),(.2,672),(.3,44),(.4,3)]
-- max similariry is .44

main = print "No main here you got mislead"
